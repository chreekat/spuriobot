{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}
-- Used for MonadConc:
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}

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
    GitLabBuildEvent (..),
    collectFailures,
    main,
    webhookAPI,
    Check(..),
    Jobbo(..),
    RetryResult(..),
) where

import Network.Wai.Handler.Warp (run)
import System.IO (hSetBuffering, stdout, BufferMode(..))
import System.Environment (
    getArgs,
    getEnv,
 )
import Control.Concurrent.Async (race_)
import Control.Concurrent.Classy (fork, MonadConc)
import Control.Monad.Catch (MonadThrow, MonadMask, MonadCatch, finally, Exception, try)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT(..), MonadReader, withReaderT, asks)
import Data.Aeson (
    FromJSON,
    parseJSON,
    withObject,
    withText,
    (.:),
    (.:?), ToJSON, toJSON,
 )
import qualified Data.Aeson.Types as Aeson
import qualified Data.Attoparsec.Text as AttoText
import qualified Data.Attoparsec.Time as Atto
import Data.ByteString (ByteString, )
import qualified Data.ByteString as BS
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Network.HTTP.Req (
    NoReqBody (..),
    bsResponse,
    defaultHttpConfig,
    headerRedacted,
    https,
    jsonResponse,
    req,
    responseBody,
    runReq,
    useHttpsURI,
    (/:), (/~), HttpException,
 )
import qualified Network.HTTP.Req as R
import Network.Wai.Middleware.RequestLogger (logStdout)
import Servant (
    JSON,
    Post,
    Proxy (..),
    ReqBody,
    Server,
    hoistServer,
    serve,
    (:>), ServerT,
 )
import qualified Text.Regex.TDFA as Regex
import Text.URI (mkURI)
import Database.PostgreSQL.Simple (Connection)
import Data.Pool (Pool, withResource, createPool)
import Data.Time (UTCTime)
import Data.Int (Int64)
import Control.Monad
import Control.Exception (throwIO, displayException)
import Control.Concurrent (Chan, writeChan, readChan, newChan)
import Data.Time.LocalTime (localTimeToUTC, utc)

import qualified Spuriobot.DB as DB
import GHC.Generics (Generic)
import Data.Void (Void)
import Control.Retry (retrying, limitRetries, fullJitterBackoff)

main :: IO ()
main = do
    -- Ensure journald gets our output
    hSetBuffering stdout NoBuffering

    args <- getArgs
    case args of
        [] -> pure ()
        (_:_) -> error "Usage: spuriobot"

    strApiToken <- encodeUtf8 . T.pack <$> getEnv "GITLAB_API_TOKEN"

    -- Die early if no DB connection. (Laziness in createPool bites us
    -- otherwise.)
    DB.close =<< DB.connect

    let fiveMin = 60 * 5
    pool <- createPool DB.connect DB.close 4 fiveMin 1

    chan <- newChan

    race_
        (runSpuriobot strApiToken pool chan retryService)
        (run 8080 $ logStdout $ serve webhookAPI (mainServer strApiToken pool chan))

spurioServer :: ServerT WebHookAPI Spuriobot
spurioServer = jobEvent

mainServer :: Token -> Pool Connection -> RetryChan -> Server WebHookAPI
mainServer tok pool chan = hoistServer webhookAPI (runSpuriobot tok pool chan) spurioServer

runSpuriobot :: MonadIO m => ByteString -> Pool Connection -> RetryChan -> Spuriobot a -> m a
runSpuriobot tok pool chan (Spuriobot act) = liftIO $ runReaderT act (SpuriobotContext tok "" pool chan)

--
-- Helpers
--

showt :: Show a => a -> Text
showt = T.pack . show

(=~) :: Text -> Text -> Bool
(=~) = (Regex.=~)



--
-- GitLab API types and handlers
--

-- | GitLab has a non-standard time format in the build events.
-- "2022-12-22 09:30:51 UTC"
newtype GitLabTime = GitLabTime UTCTime
    deriving (Eq, Show)
    deriving newtype ToJSON

instance FromJSON GitLabTime where
    parseJSON = withText "GitLabTime" (runAtto attoParseGitLabTime)
        where
        -- | Run an attoparsec parser as an aeson parser.
        -- Copied from aeson's (internal) Data.Aeson.Parser.Time.
        runAtto :: AttoText.Parser a -> Text -> Aeson.Parser a
        runAtto p t = case AttoText.parseOnly (p <* AttoText.endOfInput) t of
                    Left err -> fail $ "could not parse date: " ++ err
                    Right r  -> return r


-- | Parses "2022-12-22 09:30:51 UTC" as UTCTime
attoParseGitLabTime :: AttoText.Parser GitLabTime
attoParseGitLabTime = fmap GitLabTime $ do
    localtime <- Atto.localTime
    _ <- AttoText.string " UTC"
    pure $ localTimeToUTC utc localtime

newtype ProjectId = ProjectId {unProjectId :: Int}
    deriving stock (Show, Ord, Eq)
    deriving newtype (FromJSON, ToJSON)

-- TODO convert to newtype when the fancy strikes
type JobId = Int64
type JobWebURL = Text
type Token = ByteString

-- | The data we get from the /job API endpoint.
--
-- Most of what we need could come from the BuildEvent, but the web_url in
-- particular is missing, so it's not sufficient for our use.
--
-- Informally, we use BuildEvent to decide whether or not to check the job for
-- failures, and the /job endpoint for everything else.
data JobInfo = JobInfo
    { webUrl :: JobWebURL
    , runnerId :: Maybe Int64
    -- ^ GitLab can "lose" this information
    , jobDate :: UTCTime
    , jobFailureReason :: Maybe JobFailureReason
    }
    deriving (Show, Eq)

instance FromJSON JobInfo where
    parseJSON = withObject "JobInfo" $ \o ->
        JobInfo
            <$> o .: "web_url"
            <*> (o .:? "runner" >>= maybe (pure Nothing) (.: "id"))
            <*> o .: "created_at"
            <*> o .:? "failure_reason"

-- | Failure reasons that we care about.
data JobFailureReason = JobTimeout | JobStuck | OtherReason Text
    deriving (Eq, Show)

instance FromJSON JobFailureReason where
    parseJSON = withText "JobFailureReason" (pure . f) where
        f "job_execution_timeout" = JobTimeout
        f "stuck_or_timeout_failure" = JobStuck
        f x = OtherReason x

-- | The known build statuses that we care about.
data BuildStatus = Failed | OtherBuildStatus Text
    deriving (Eq, Show)

instance FromJSON BuildStatus where
    parseJSON = withText "BuildStatus" (pure . f) where
        f "failed" = Failed
        f x = OtherBuildStatus x

instance ToJSON BuildStatus where
    toJSON Failed = "failed"
    toJSON (OtherBuildStatus x) = toJSON x

-- | BuildEvent is what the webhook receives
data GitLabBuildEvent = GitLabBuildEvent
    { glbBuildId :: Int64
    , glbBuildStatus :: BuildStatus
    , glbProjectId :: ProjectId
    , glbFinishedAt :: Maybe GitLabTime
    }
    deriving stock (Show, Eq, Generic)

instance ToJSON GitLabBuildEvent

instance FromJSON GitLabBuildEvent where
    parseJSON = withObject "GitLabBuildEvent" $ \v ->
        GitLabBuildEvent
            <$> v .: "build_id"
            <*> v .: "build_status"
            <*> v .: "project_id"
            <*> v .: "build_finished_at"

-- | Get /jobs/<job-id>
fetchJobInfo :: ProjectId -> JobId -> Spuriobot JobInfo
fetchJobInfo (ProjectId projectId) jobId = do
    tok <- asks apiToken
    fmap responseBody $ runReq defaultHttpConfig $
        req
            R.GET
            ( https
                "gitlab.haskell.org"
                /: "api"
                /: "v4"
                /: "projects"
                /~ projectId
                /: "jobs"
                /~ jobId
            )
            NoReqBody
            jsonResponse
            (headerRedacted "PRIVATE-TOKEN" tok)




--
-- Retry handling
--

type RetryChan = Chan RetryAction
type RetryMap = M.Map JobId Word

data RetryAction = RetryAction JobId TraceContext RetryCmd
data RetryCmd
    = Retry ProjectId
    -- ^ One needs to know the project id for a job to send a retry to GitLab
    | Clear

clearRetry :: JobId -> Spuriobot ()
clearRetry jobId = do
    chan <- asks retryChan
    ctx <- asks traceContext
    liftIO $ writeChan chan (RetryAction jobId ctx Clear)

retryJob :: ProjectId -> JobId -> Spuriobot ()
retryJob projId jobId = do
    chan <- asks retryChan
    ctx <- asks traceContext
    liftIO $ writeChan chan (RetryAction jobId ctx (Retry projId))

-- | This is an action that continually pulls from the chan and handles
-- 'RetryCmd' commands.
--
-- This "service" will keep running even if GitLab requests experience HTTP
-- failures. Other kinds of exceptions, however, will crash the thread, which
-- will bubble up and crashes the whole app. systemd will need to take over in
-- that case.
retryService :: Spuriobot Void
retryService = loop M.empty where
    -- Number of times we will retry a job.
    maxRetries = 10

    loop :: RetryMap -> Spuriobot Void
    loop retryMap = do
        RetryAction jobId ctx cmd <- liftIO . readChan =<< asks retryChan
        -- A job that hasn't been retried isn't in the map, so we return 0
        let retryCount = M.findWithDefault 0 jobId retryMap
        -- We're done with this entry now, so delete it. Note this makes 'Clear'
        -- a no-op in handle_cmd.
        let retryMap' = M.delete jobId retryMap
        loop =<< withTrace ctx (handle_cmd jobId retryMap' retryCount cmd)

    handle_cmd jobId retryMap retryCount cmd = do
        case cmd of
            Clear -> pure retryMap
            Retry projId ->
                if retryCount < maxRetries
                then do
                    newId' <- retry_job projId jobId
                    case newId' of
                        Just newId -> do
                            trace $ "retried as job: " <> showt newId
                            pure (M.insert newId (retryCount + 1) retryMap)
                        Nothing -> pure retryMap
                else do
                    trace "exceeded max retries"
                    pure retryMap

    retry_job :: ProjectId -> JobId -> Spuriobot (Maybe JobId)
    retry_job (ProjectId projectId) jobId =
        let -- Test if we should retry the retry action.
            test_exception (Left e) = do
                withTrace "WARN" $ do
                    trace "Retry failed."
                    mapM_ trace (T.lines . T.pack . displayException $ e)
                pure True
            test_exception (Right _) = pure False


            -- This sends the retry command to GitLab.
            -- The type signature is crucial as it constrains `try`.
            retry_action :: Spuriobot (Either HttpException JobId)
            retry_action = try $ do
                tok <- asks apiToken
                fmap (retryJobId . responseBody) $ runReq defaultHttpConfig $
                    req
                        R.POST
                        retryUrl
                        NoReqBody
                        jsonResponse
                        (headerRedacted "PRIVATE-TOKEN" tok)

                where retryUrl =
                        https "gitlab.haskell.org"
                            /: "api"
                            /: "v4"
                            /: "projects"
                            /~ projectId
                            /: "jobs"
                            /~ jobId
                            /: "retry"

            policy = limitRetries 5 <> fullJitterBackoff halfSecond
                where halfSecond = 500 * 1000
        in do
            res <- retrying policy (const test_exception) (const retry_action)
            case res of
                Left _ -> Nothing <$ trace ("ERR: Giving up retrying job " <> showt jobId)
                Right j -> pure $ Just j

newtype RetryResult = RetryResult { retryJobId :: Int64 }
    deriving (Eq, Show)

instance FromJSON RetryResult where
    parseJSON = withObject "RetryResult" $ \o ->
        RetryResult <$> o .: "id"
--
-- Servant boilerplate
--

type WebHookAPI =
    ReqBody '[JSON] GitLabBuildEvent :> Post '[JSON] ()

webhookAPI :: Proxy WebHookAPI
webhookAPI = Proxy

-- | This turns the job processor into a webhook endpoint by immediately forking
-- to do the real work.
--
-- See the recommendations from GitLab documentation:
-- https://docs.gitlab.com/ee/user/project/integrations/webhooks.html#configure-your-webhook-receiver-endpoint
jobEvent :: GitLabBuildEvent -> Spuriobot ()
jobEvent glBuildEvent =
    void $ fork $ withTrace (showt (glbBuildId glBuildEvent)) $ processBuildEvent glBuildEvent




--
-- Handler context setup
--

-- Version 0 of tracing is running handlers in a context where there's a logging
-- context we can use to decorate traces.

data SpuriobotContext = SpuriobotContext
    { apiToken :: ByteString
    , traceContext :: TraceContext
    , dbPool :: Pool Connection
    , retryChan :: RetryChan
    }

type TraceContext = Text

newtype Spuriobot a = Spuriobot { unSpuriobot :: ReaderT SpuriobotContext IO a }
    deriving newtype (Functor, Applicative, Monad, MonadReader SpuriobotContext, MonadIO, MonadThrow, MonadCatch, MonadMask, MonadConc)

-- | Spuriobot combinator that adds to the trace context.
withTrace :: Text -> Spuriobot a -> Spuriobot a
withTrace t_  = Spuriobot . withReaderT (modifyTraceContext (addContext t_)) . unSpuriobot
    where
        addContext t old_t
            | T.null old_t = t
            | otherwise = old_t <> ":" <> t
        modifyTraceContext f sc = sc { traceContext = f (traceContext sc) }

-- | Spuriobot action that prints a trace message in the current trace context.
-- Uses BS.putStr to prevent interleaving.
trace :: Text -> Spuriobot ()
trace t = withTrace t (liftIO . BS.putStr . encodeUtf8 . (<> "\n") =<< asks traceContext)

-- | Spuriobot action that runs a database action.
runDB :: (Connection -> IO a) -> Spuriobot a
runDB db_act =
    let run_ pool = liftIO $ withResource pool db_act
    in withTrace "db" $ run_ =<< asks dbPool








--
-- Controller logic
--

data SpuriobotException = ParseUrlFail
    deriving stock (Eq, Show)
    deriving anyclass Exception

-- | Get the raw job trace.
fetchJobLogs :: JobWebURL -> Spuriobot Text
fetchJobLogs jobWebURL = do
    let mbUri = do
            uri <- mkURI (jobWebURL <> "/raw")
            (uri', _) <- useHttpsURI uri
            pure uri'
    case mbUri of
        -- this error will only terminate the forked processFailure thread, so the
        -- main process should keep listening and handling requests
        -- FIXME: use throwError in the Handler monad
        Nothing -> do
            trace $ "error: could not parse URL: " <> jobWebURL
            liftIO $ throwIO ParseUrlFail
        Just uri -> do
            tok <- asks apiToken
            runReq defaultHttpConfig $ do
                -- TODO handle redirect to login which is gitlab's way of saying 404
                -- if re.search('users/sign_in$', resp.url):
                response <-
                    req
                        R.GET
                        uri
                        NoReqBody
                        bsResponse
                        (headerRedacted "PRIVATE-TOKEN" tok)

                pure . decodeUtf8 . responseBody $ response

-- the code that we inject into the database
type FailureErrorCode = Text

-- the message we echo to stdout
type FailureMessage = Text

type Failure = (FailureErrorCode, FailureMessage)

data Check = Check
    { checkMsg :: FailureMessage
    , checkCode :: FailureErrorCode
    , checkFn :: Jobbo -> Bool
    }

runCheck :: Jobbo -> Check -> Maybe (FailureMessage, FailureErrorCode)
runCheck j (Check msg cod fn) = if fn j then Just (cod, msg) else Nothing

checkTimeout :: Check
checkTimeout = Check "job timeout" "job_timeout" $ \(Jobbo rs _) ->
    case rs of
        Just JobTimeout -> True
        Just JobStuck -> True
        Just (OtherReason _) -> False
        Nothing -> False

checkLogs :: [Check]
checkLogs =
    let (Jobbo _ logs) !> search = search `T.isInfixOf` logs
        (Jobbo _ logs) ~> search = logs =~ search
    in
        [ Check "docker failure" "docker"
            (!> "Cannot connect to the Docker daemon at unix:///var/run/docker.sock")
        -- TODO: Disabled until the bot gains backoff abilities.
        --, Check "image pull failure" "pull_image"
        --    (!> "failed to pull image \"registry.gitlab.haskell.org")
        , Check "GitLab connection failure" "connect_gitlab"
            (!> "Failed to connect to gitlab.haskell.org")
        , Check "exhausted disk" "no_space"
            (\j -> j !> "No space left on device"
                -- Avoid false positives from T21336 output.
                -- This may cause false negatives, but I think that's the lesser
                -- of two evils.
                && not (j !> "GHC.IO.FD.fdWrite: resource exhausted"
                        || j !> "<stdout>: hFlush: resource exhausted"))
        -- head.hackage#38
        , Check "received signal 9" "signal_9"
            (~> "failed due to signal 9 .Killed")
        -- Modified this search due to ghc#23139. See #9.
        , Check "could not allocate memory" "cannot_allocate"
            (~> "osCommitMemory: VirtualAlloc MEM_COMMIT failed|out of memory allocating \\d+ bytes")
        , Check "MoveFileEx-related failure" "MoveFileEx"
            (!> "MoveFileEx")
        -- #23039
        , Check "Submodule clone failure" "submodule_clone"
            (~> "Failed to clone '.*' a second time, aborting")
        -- #22870
        , Check "ghc-pkg or hadrian failure" "ghc-pkg_died"
            (!> "ghc-pkg dump failed: dieVerbatim: user error")
        -- #22860
        , Check "Nix#7273 failure" "nix_T7273"
            (\t -> t !> "cannot link '/nix/store/.tmp-link"
                || t !> "error: clearing flags of path '/nix/store")
        -- #22408
        , Check "\"cabal exec hadrian\" segfault" "cabal_hadrian_segfault"
            (~> "Segmentation fault.*CABAL.*new-exec.*hadrian")
        -- #22869
        , Check "error code: -6" "code_-6"
            (~> "Command failed with error code: -6")
        , Check "runner terminated" "runner_process_terminated"
            (!> "ERROR: Job failed (system failure): aborted: terminated")
        -- #22967
        , Check "ghc-config file conflict" "ghc-config_file_conflict"
            (~> "posix_spawnp: resource busy")
        -- #22990
        , Check "error code: -11" "code_-11"
            (~> "Command failed with error code: -11")
        -- #21008
        , Check "ulimit: Invalid argument" "ulimit"
            (~> "ulimit: virtual memory: cannot modify limit: Invalid argument")
        -- #23039
        , Check "error cloning fresh repository" "repo_clone"
            (!> "fresh repository.\x1b[0;m\nerror")
        -- #23039 again
        , Check "error fetch perf notes" "perf_note_fetch"
            (!> "refs/notes/perf:refs/notes/perf\nerror:")
        -- #23144
        , Check "death by SIGQUIT" "sigquit"
            (~> "^SIGQUIT: quit")
        ]

-- TODO tests
collectFailures :: Jobbo -> Set Failure
collectFailures j = S.fromList (mapMaybe (runCheck j) (checkTimeout : checkLogs))

logFailures :: Set Failure -> Spuriobot ()
logFailures failures
    | S.null failures = trace "no known spurio"
    | otherwise = forM_
        (S.toList failures)
        ( \(_, msg) -> trace msg)

-- | Top-level handler for the GitLab job event
-- https://docs.gitlab.com/ee/user/project/integrations/webhook_events.html#job-events
processBuildEvent :: GitLabBuildEvent -> Spuriobot ()
processBuildEvent ev = do
    case glbFinishedAt ev of
        Nothing -> trace "skipping unfinished job"
        -- FIXME explain use of clearRetry here.
        Just _ -> withTrace "finished" $ processFinishedJob ev `finally` clearRetry (glbBuildId ev)

processFinishedJob :: GitLabBuildEvent -> Spuriobot ()
processFinishedJob ev = do
    case glbBuildStatus ev of
        OtherBuildStatus x -> trace x
        Failed -> withTrace "failed" $ processFailure ev

-- | Characteristics of a job that we test against.
data Jobbo = Jobbo (Maybe JobFailureReason) Text

-- | Given a failed job, deal with spurios (if any)
processFailure :: GitLabBuildEvent -> Spuriobot ()
processFailure GitLabBuildEvent { glbProjectId, glbBuildId } = do
    jobInfo <- fetchJobInfo glbProjectId glbBuildId
    logs <- fetchJobLogs (webUrl jobInfo)
    let jobbo = Jobbo (jobFailureReason jobInfo) logs
    let failures = collectFailures jobbo
    logFailures failures
    void $ runDB $ DB.insertFailures (mkDBFailures glbBuildId jobInfo (S.toList failures))
    unless (S.null failures) $
        withTrace "retrying" $ retryJob glbProjectId glbBuildId

-- | Map between our types and the DB's types
mkDBFailures :: Functor f => Int64 -> JobInfo -> f Failure -> f DB.Failure
mkDBFailures jobId JobInfo { jobDate, webUrl, runnerId } fails =
    let mk (code, _) = DB.Failure jobId code jobDate webUrl runnerId
    in fmap mk fails
