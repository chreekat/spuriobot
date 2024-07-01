{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
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

import Control.Monad.Catch (Exception)
import Network.Wai.Handler.Warp (run)
import System.IO (hSetBuffering, stdout, BufferMode(..))
import System.Environment (
    getArgs,
    getEnv,
 )
import Control.Concurrent.Async (race_)
import Control.Concurrent.Classy (fork, getNumCapabilities)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Servant
import Database.PostgreSQL.Simple (Connection)
import Data.Pool (Pool, newPool, defaultPoolConfig)
import Control.Monad (void)
import Control.Concurrent (newChan)
import Control.Concurrent.STM (newTMVarIO, TMVar)
import qualified Database.SQLite.Simple as SQLite


import qualified Spuriobot.DB as DB
import GitLabApi
import Spuriobot.RetryJob
import Spuriobot.Foundation
import Spuriobot.Spurio
import Control.Monad.Catch (finally)
import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (asks)


-- import System.Environment (getArgs)
import Data.Time (UTCTime, getCurrentTime, addUTCTime, nominalDay, parseTimeM, defaultTimeLocale)
import Control.Exception (SomeException, handle, throwIO)
import GitLabJobs (fetchJobsBetweenDates, initDatabase)
import Text.URI (render)

-- | API served by this app
type WebHookAPI =
    "spuriobot" :> ReqBody '[JSON] GitLabBuildEvent :> Post '[JSON] ()
    :<|> "spuriobot_system" :> ReqBody '[JSON] GitLabSystemEvent :> Post '[JSON] ()
    -- Backward compat top-level endpoint duplicating the "spuriobot" endpoint.
    :<|> ReqBody '[JSON] GitLabBuildEvent :> Post '[JSON] ()

webhookAPI :: Proxy WebHookAPI
webhookAPI = Proxy

data DateParseException = DateParseException String
    deriving (Show)

instance Exception DateParseException

main :: IO ()
main = do
    -- Ensure journald gets our output
    hSetBuffering stdout NoBuffering

    -- args <- getArgs
    -- case args of
    --     [] -> pure ()
    --     (_:_) -> error "Usage: spuriobot"

    args <- getArgs
    case args of
        ["fetchjobs", startStr, endStr] -> handle dateException $ handle allExceptions $ do
            let startDate = parseDate startStr
            let endDate = parseDate endStr
            case (startDate, endDate) of
                (Right start, Right end) -> fetchJobsBetweenDates (start, end)
                (Left err, _) -> throwIO $ DateParseException err
                (_, Left err) -> throwIO $ DateParseException err
        ["fetchjobs"] -> do
            now <- getCurrentTime
            let lastMonth = addUTCTime (-30 * nominalDay) now
            fetchJobsBetweenDates (lastMonth, now)
        _ -> do
            strApiToken <- GitLabToken . encodeUtf8 . T.pack <$> getEnv "GITLAB_API_TOKEN"

            -- Die early if no DB connection. (Laziness in createPool bites us
            -- otherwise.)
            DB.close =<< DB.connect
            connVar <- newTMVarIO =<< SQLite.open "jobs.db"
            initDatabase connVar

            let fiveMin = 60 * 5
            -- See https://github.com/scrive/pool/issues/31#issuecomment-2043213626
            reasonableDefault <- getNumCapabilities
            pool <- newPool (defaultPoolConfig DB.connect DB.close fiveMin reasonableDefault)

            chan <- RetryChan <$> newChan

            race_
                (runSpuriobot strApiToken pool chan connVar retryService)
                (run 8080 $ logStdout $ serve webhookAPI (mainServer strApiToken pool chan connVar))
    where
        parseDate :: String -> Either String UTCTime
        parseDate str =
            case parseTimeM True defaultTimeLocale "%Y-%m-%d" str of
                Just date -> Right date
                Nothing -> Left $ "Invalid date format for: " ++ str

        dateException :: DateParseException -> IO ()
        dateException (DateParseException msg) = putStrLn $ "Error parsing dates: " ++ msg

        allExceptions :: SomeException -> IO ()
        allExceptions e = putStrLn $ "An error occurred: " ++ show e

    

spurioServer :: ServerT WebHookAPI Spuriobot
spurioServer = jobEvent :<|> systemEvent :<|> jobEvent
    where
        jobEvent = mkHook (showt . glbBuildId) processBuildEvent
        systemEvent = mkHook (const "system") processSystemEvent

-- mainServer :: GitLabToken -> Pool Database.PostgreSQL.Simple.Connection -> RetryChan -> TMVar SQLite.Connection -> Server WebHookAPI
-- mainServer tok pool chan = hoistServer webhookAPI (runSpuriobot tok pool chan connVar) spurioServer
mainServer :: GitLabToken -> Pool Database.PostgreSQL.Simple.Connection -> RetryChan -> TMVar SQLite.Connection -> Server WebHookAPI
mainServer tok pool chan connVar = hoistServer webhookAPI (nt tok pool chan connVar) spurioServer
  where
    nt :: GitLabToken -> Pool Database.PostgreSQL.Simple.Connection -> RetryChan -> TMVar SQLite.Connection -> Spuriobot a -> Handler a
    nt tok pool chan connVar action = liftIO $ runSpuriobot tok pool chan connVar action

-- | This turns the a request processor into a webhook endpoint by immediately
-- forking to do the real work.
--
-- See the recommendations from GitLab documentation:
-- https://docs.gitlab.com/ee/user/project/integrations/webhooks.html#configure-your-webhook-receiver-endpoint
mkHook
    :: (req -> TraceContext) -- ^ Generate a trace context from the request
    -> (req -> Spuriobot ()) -- ^ Process the request
    -> req
    -> Spuriobot ()
mkHook ctx fn req = void $ fork $ withTrace (ctx req) $ fn req

-- | Top-level handler for the GitLab job event
-- https://docs.gitlab.com/ee/user/project/integrations/webhook_events.html#job-events
processBuildEvent :: GitLabBuildEvent -> Spuriobot ()
processBuildEvent ev = do
    case glbFinishedAt ev of
        Nothing -> trace "skipping unfinished job"
        -- FIXME explain use of clearRetry here.
        Just _ -> withTrace "finished" $ processFinishedJob ev `finally` clearRetry (glbBuildId ev)

-- processFinishedJob :: GitLabBuildEvent -> Spuriobot ()
-- processFinishedJob ev = do
--     insertLogtoFTS ev
--     -- Handle specific job statuses
--     case glbBuildStatus ev of
--         OtherBuildStatus x -> trace x
--         Failed -> withTrace "failed" $ processFailure ev


data SpuriobotException = ParseUrlFail
    deriving stock (Eq, Show)
    deriving anyclass (Exception)

processFinishedJob :: GitLabBuildEvent -> Spuriobot ()
processFinishedJob ev = do
    logs <- fetchLogsForEvent ev
    insertLogtoFTS ev logs
    -- Handle specific job statuses
    case glbBuildStatus ev of
        OtherBuildStatus x -> trace x
        Failed -> withTrace "failed" $ processFailure ev logs

fetchLogsForEvent :: GitLabBuildEvent -> Spuriobot Text
fetchLogsForEvent ev = do
    tok <- asks apiToken
    let projectId = glbProjectId ev
        jobId = glbBuildId ev
    jobInfo <- liftIO $ fetchFinishedJob tok projectId jobId
    l <- liftIO $ fetchJobLogs tok (webUrl jobInfo)
    case l of
        Left (JobWebUrlParseFailure url) -> do
            trace $ "error: could not parse URL: " <> render url
            liftIO $ throwIO ParseUrlFail
        Right logs -> pure logs

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
