{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{- |
Module: Spuriobot
Description: Log spurious GHC GitLab build failures

Overall flow of the application:
- Receive a build event webhook call from GitLab
- Fetch the job log for that event
- Search in the log for known patterns for spurious failures
- Log the results to stdout
- TODO: write results to database
- TODO: tell the job to retry
-}
module Spuriobot (
    main,
) where

import Control.Concurrent (newChan)
import Control.Concurrent.Async (race_)
import Control.Concurrent.Classy (fork, getNumCapabilities)
import Control.Concurrent.STM (newTMVarIO, TMVar)
import Control.Exception (handle, throwIO)
import Control.Monad (void)
import Control.Monad.Catch ( Exception, finally, MonadThrow (..) )
import Control.Monad.Reader (asks)
import Data.Pool (Pool, newPool, defaultPoolConfig)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Time (UTCTime, getCurrentTime, addUTCTime, nominalDay, parseTimeM, defaultTimeLocale)
import Database.PostgreSQL.Simple (Connection)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Servant
import System.Environment ( getArgs, getEnv,)
import System.IO (hSetBuffering, stdout, BufferMode(..), hPutStrLn, stderr)
import qualified Data.Text as T
import qualified Database.SQLite.Simple as SQLite

import qualified Spuriobot.DB as DB
import GitLabApi
import Spuriobot.RetryJob
import Spuriobot.Foundation
import Spuriobot.Spurio
import Spuriobot.SearchUI (searchUIServer)
import Web.Scotty
import Spuriobot.Backfill (fetchJobsBetweenDates, initDatabase, insertLogtoFTS)
import qualified Network.HTTP.Req as R
import Network.HTTP.Req (https, headerRedacted, (/:), (/~))
import Data.Maybe (fromJust)


-- | API served by this app
type WebHookAPI =
    "spuriobot" :> ReqBody '[JSON] GitLabBuildEvent :> Post '[JSON] ()
    :<|> "spuriobot_system" :> ReqBody '[JSON] GitLabSystemEvent :> Post '[JSON] ()
    -- Backward compat top-level endpoint duplicating the "spuriobot" endpoint.
    :<|> ReqBody '[JSON] GitLabBuildEvent :> Post '[JSON] ()

webhookAPI :: Proxy WebHookAPI
webhookAPI = Proxy

newtype DateParseException = DateParseException String
    deriving (Show)

instance Exception DateParseException

main :: IO ()
main = do
    -- Ensure journald gets our output
    hSetBuffering stdout NoBuffering

    args <- getArgs
    now <- getCurrentTime
    let lastMonth = addUTCTime (-30 * nominalDay) now
    case args of
        ["fetchjobs", startStr, endStr] -> handle dateException $ do
            let startDate = parseDate startStr
            let endDate = parseDate endStr
            case (startDate, endDate) of
                (Right start, Right end) -> fetchJobsBetweenDates (start, end)
                (Left err, _) -> throwIO $ DateParseException err
                (_, Left err) -> throwIO $ DateParseException err
        ["fetchjobs", startStr] -> handle dateException $ do
            let startDate = parseDate startStr
            case startDate of
                Right start -> fetchJobsBetweenDates (start, now)
                Left err -> throwIO $ DateParseException err
        ["fetchjobs"] -> do
            fetchJobsBetweenDates (lastMonth, now)
        [] -> do
            strApiToken <- GitLabToken . encodeUtf8 . T.pack <$> getEnv "GITLAB_API_TOKEN"

            -- Die early if no DB connection. (Laziness in createPool bites us
            -- otherwise.)
            DB.close =<< DB.connect
            connVar' <- newTMVarIO =<< SQLite.open "jobs.db"
            initDatabase connVar'

            let fiveMin = 60 * 5
            -- See https://github.com/scrive/pool/issues/31#issuecomment-2043213626
            reasonableDefault <- getNumCapabilities
            pool <- newPool (defaultPoolConfig DB.connect DB.close fiveMin reasonableDefault)

            chan <- RetryChan <$> newChan
            scotty 3000 $ searchUIServer connVar'
            race_
                (runSpuriobot strApiToken pool chan connVar' retryService)
                (run 8080 $ logStdout $ serve webhookAPI (mainServer strApiToken pool chan connVar'))
        _ -> hPutStrLn stderr $ unlines
            [ ""
            , "spuriobot: Spurious failure tool for CI"
            , ""
            , "Usage as webhook:"
            , "    spuriobot"
            , ""
            , "Usage as tool for backfilling job logs:"
            , "    spuriobot fetchjobs [start [end]]"
            , ""
            , "    Dates are in the format YYYY-MM-DD."
            , "    Default start is 30 days ago."
            , "    Default end is now."
            ]

    where
        parseDate :: String -> Either String UTCTime
        parseDate str =
            case parseTimeM True defaultTimeLocale "%Y-%m-%d" str of
                Just date -> Right date
                Nothing -> Left $ "Invalid date format for: " ++ str

        dateException :: DateParseException -> IO ()
        dateException (DateParseException msg) = putStrLn $ "Error parsing dates: " ++ msg

spurioServer :: ServerT WebHookAPI Spuriobot
spurioServer = jobEvent :<|> systemEvent :<|> jobEvent
    where
        jobEvent = mkHook (showt . glbBuildId) processBuildEvent
        systemEvent = mkHook (const "system") processSystemEvent

mainServer :: GitLabToken -> Pool Database.PostgreSQL.Simple.Connection -> RetryChan -> TMVar SQLite.Connection -> Server WebHookAPI
mainServer tok pool chan connVar' = hoistServer webhookAPI (nt tok pool chan connVar') spurioServer
  where
    nt :: GitLabToken -> Pool Database.PostgreSQL.Simple.Connection -> RetryChan -> TMVar SQLite.Connection -> Spuriobot a -> Servant.Handler a
    nt t p c cv action = liftIO $ runSpuriobot t p c cv action

-- | This turns the a request processor into a webhook endpoint by immediately
-- forking to do the real work.
--
-- See the recommendations from GitLab documentation:
-- https://docs.gitlab.com/ee/user/project/integrations/webhooks.html#configure-your-webhook-receiver-endpoint
mkHook
    :: (request -> TraceContext) -- ^ Generate a trace context from the request
    -> (request -> Spuriobot ()) -- ^ Process the request
    -> request
    -> Spuriobot ()
mkHook ctx fn rq = void $ fork $ withTrace (ctx rq) $ fn rq

-- | Top-level handler for the GitLab job event
-- https://docs.gitlab.com/ee/user/project/integrations/webhook_events.html#job-events
processBuildEvent :: GitLabBuildEvent -> Spuriobot ()
processBuildEvent ev = do
    case glbFinishedAt ev of
        Nothing -> trace "skipping unfinished job"
        -- (A year later) Uh oh... I don't remember what the reason was for
        -- 'clearRetry' here. Maybe it's to clear the retry counter for retried
        -- jobs that succeeded.
        Just _ -> withTrace "finished" $ processFinishedJob ev `finally` clearRetry (glbBuildId ev)

data SpuriobotException = ParseUrlFail
    deriving stock (Eq, Show)
    deriving anyclass (Exception)

gitlab :: R.Url 'R.Https
gitlab = https "gitlab.haskell.org" /: "api" /: "v4"

-- | Local definition of 'R.req' that runs in Spuriobot.
-- Passes the gitlab root url to the second argument and provides the
-- PRIVATE-TOKEN header automatically.
req
    :: (R.HttpBodyAllowed (R.AllowsBody method) (R.ProvidesBody body), R.HttpResponse a, R.HttpMethod method, R.HttpBody body)
    => method
    -> (R.Url 'R.Https -> R.Url scheme)
    -> body
    -> Proxy a
    -> R.Option scheme
    -> Spuriobot (R.HttpResponseBody a)
req meth mkUrl body ty opts = do
    GitLabToken tok <- asks apiToken
    R.responseBody <$> R.req meth (mkUrl gitlab) body ty (headerRedacted "PRIVATE-TOKEN" tok <> opts)

-- | Currently we have to make 3 separate requests to get all the data we need
-- about a job.
fetchFinishedJob :: ProjectId -> JobId -> Spuriobot FinishedJob
fetchFinishedJob p@(ProjectId projId) jobId = do
    job <- req R.GET (\f -> f /: "projects" /~ projId /: "jobs" /~ jobId) R.NoReqBody R.jsonResponse mempty
    projInfo <- req R.GET (\f -> f /: "projects" /~ projId) R.NoReqBody R.jsonResponse mempty
    logs <- fetchJobLogs (webUrl job)
    pure FinishedJob
        { finishedJobWebUrl = webUrl job
        , finishedJobRunnerId = runnerId job
        , finishedJobRunnerName = runnerName job
        , finishedJobCreatedAt = jobCreatedAt job
        , finishedJobFinishedAt = fromJust $ jobFinishedAt job
        , finishedJobFailureReason = jobFailureReason job
        , finishedJobName = jobName job
        , finishedJobProjectId = p
        , finishedJobProjectPath = projPath projInfo
        , finishedJobLogs = logs
        , finishedJobId = jobId
        }

-- | Once a job is finished, we need to add its logs to the FTS database, and we
-- need to retry any spurious failure. The first step is to gather all the data
-- we need about the job. In GitLab that means we need to fetch the logs and
-- some project info separately.
processFinishedJob :: GitLabBuildEvent -> Spuriobot ()
processFinishedJob GitLabBuildEvent { glbProjectId, glbBuildId, glbBuildStatus } = do
    finishedJob <- withTrace "fetch job" $ fetchFinishedJob glbProjectId glbBuildId
    withTrace "insert to fts" $ insertLogtoFTS finishedJob
    -- Handle specific job statuses
    case glbBuildStatus of
        OtherBuildStatus x -> trace x
        Failed -> withTrace "failed" $ processFailure finishedJob

-- | Get the raw job trace. The JobWebUrl may fail to parse, but we'll just kill
-- the thread if so. Hm wait I log that, right?
--
-- This function uses the web UI rather than the actual GitLab JSON API. That's
-- because the trace endpoint of the API is very slow.
fetchJobLogs :: JobWebURI -> Spuriobot Text
fetchJobLogs (JobWebURI jobURI) =
    let url = R.useURI jobURI
    in case url of
        -- lol
        Just (Left (url', opt)) -> fetch_job_raw url' opt
        Just (Right (url', opt)) -> fetch_job_raw url' opt
        Nothing -> throwM (JobWebUrlParseFailure jobURI)

    where
        fetch_job_raw url opt =
            let raw_url = url /: "raw"
            in do
                -- TODO handle redirect to login which is gitlab's way of saying 404
                -- if re.search('users/sign_in$', resp.url):
                (GitLabToken tok) <- asks apiToken
                response <-
                    R.req
                        R.GET
                        raw_url
                        R.NoReqBody
                        R.bsResponse
                        (opt <> headerRedacted "PRIVATE-TOKEN" tok)

                pure . decodeUtf8 . R.responseBody $ response

-- | Top-level handler for GitLab system events
-- https://gitlab.haskell.org/help/administration/system_hooks
processSystemEvent :: GitLabSystemEvent -> Spuriobot ()
processSystemEvent (ProjectSystemEvent ProjectCreate projId) =
    withTrace ("project " <> showt (unProjectId projId)) (installHook projId)
processSystemEvent (ProjectSystemEvent OtherProjectEvent projId) =
    withTrace ("project " <> showt (unProjectId projId)) (trace "skipping other project event")
processSystemEvent OtherSystemEvent = trace "skipping other system event"

installHook :: ProjectId -> Spuriobot ()
installHook projId = do
    tok <- asks apiToken
    liftIO $ addProjectBuildHook tok projId "http://127.0.0.1:8080/spuriobot"
    trace "hook installed"

--
-- Helpers
--

showt :: Show a => a -> Text
showt = T.pack . show
