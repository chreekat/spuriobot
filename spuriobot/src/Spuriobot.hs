{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
    GitLabBuildEvent (..),
    collectFailures,
    main,
    webhookApplication,
    webhookAPI,
    Check(..),
    Jobbo(..),
) where

import Network.Wai.Handler.Warp (run)
import System.IO (hSetBuffering, stdout, BufferMode(..))
import System.Environment (
    getArgs,
    getEnv,
 )
import Control.Concurrent (forkIO)
import Control.Exception (throwIO, Exception)
import Control.Monad (
    forM_,
    void,
 )
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT(..), MonadReader, withReaderT, asks)
import Data.Aeson (
    FromJSON,
    ToJSON,
    object,
    parseJSON,
    toJSON,
    withObject,
    withText,
    (.:),
    (.:?),
    (.=),
 )
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.Text.IO as T
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
    (/:),
 )
import qualified Network.HTTP.Req as R
import Servant (
    Application,
    Handler,
    JSON,
    Post,
    Proxy (..),
    ReqBody,
    serve,
    (:>),
 )
import qualified Text.Regex.TDFA as Regex
import Text.URI (mkURI)
import Database.PostgreSQL.Simple (Connection)
import Data.Pool (Pool, withResource, createPool)
import Data.Time (UTCTime)

import qualified Spuriobot.DB as DB
import Data.Int (Int64)

main :: IO ()
main = do
    -- Ensure journald gets our output
    hSetBuffering stdout NoBuffering

    -- Fail early if the database connection doesn't work.
    -- Actually, I need to set up the pool here and make sure it gets shared
    -- among all requests. Right now, the pool is created afresh for each
    -- request...
    DB.close =<< DB.connect

    args <- getArgs
    envStrApiToken <- getEnv "GITLAB_API_TOKEN"
    case envStrApiToken of
        "" -> error "please set the GITLAB_API_TOKEN environment variable to a valid token string"
        strApiToken ->
            case args of
                [] -> run 8080 $ webhookApplication (textEncode strApiToken)
                _ -> error "Usage: spuriobot"
  where
    textEncode = encodeUtf8 . T.pack
--
-- Helpers
--

showt :: Show a => a -> Text
showt = T.pack . show

(=~) :: Text -> Text -> Bool
(=~) = (Regex.=~)

--
-- Gitlab API types and handlers
--

newtype ProjectId = ProjectId {unProjectId :: Int}
    deriving stock (Show, Ord, Eq)
    deriving newtype (FromJSON, ToJSON)

-- TODO convert to newtype when the fancy strikes
type JobId = Int
type JobWebURL = Text
type Token = ByteString

-- Sparse definition
newtype JobInfo = JobInfo
    { webUrl :: JobWebURL
    }
    deriving (Show, Ord, Eq)

instance FromJSON JobInfo where
    parseJSON = withObject "JobInfo" $ \o ->
        JobInfo <$> o .: "web_url"

-- | The known build statuses that we care about.
data BuildStatus = Failed | OtherBuildStatus Text
    deriving (Eq, Show)

instance FromJSON BuildStatus where
    parseJSON = withText "BuildStatus" (pure . f) where
        f "failed" = Failed
        f x = OtherBuildStatus x

instance ToJSON BuildStatus where
    toJSON Failed = Aeson.String "failed"
    toJSON (OtherBuildStatus x) = Aeson.String x

-- BuildEvent is what the webhook receives
data GitLabBuildEvent = GitLabBuildEvent
    { glbBuildId :: Int
    , glbBuildName :: Text
    , glbBuildStatus :: BuildStatus
    , glbProjectId :: ProjectId
    , glbJobFailureReason :: Maybe JobFailureReason
    }
    deriving (Show, Eq)

instance FromJSON GitLabBuildEvent where
    parseJSON = withObject "GitLabBuildEvent" $ \v ->
        GitLabBuildEvent
            <$> v .: "build_id"
            <*> v .: "build_name"
            <*> v .: "build_status"
            <*> v .: "project_id"
            <*> v .:? "failure_reason"

instance ToJSON GitLabBuildEvent where
    toJSON x =
        object
            [ "build_id" .= glbBuildId x
            , "build_name" .= glbBuildName x
            , "build_status" .= glbBuildStatus x
            , "project_id" .= glbProjectId x
            ]

data JobFailureReason = JobTimeout | JobStuck | OtherReason Text
    deriving (Eq, Show)

instance FromJSON JobFailureReason where
    parseJSON = withText "JobFailureReason" (pure . f) where
        f "job_execution_timeout" = JobTimeout
        f "stuck_or_timeout_failure" = JobStuck
        f x = OtherReason x

-- | Get /jobs/<job-id>
fetchJobInfo :: Token -> ProjectId -> JobId -> Spuriobot JobInfo
fetchJobInfo apiToken (ProjectId projectId) jobId = liftIO $ runReq defaultHttpConfig $ do
    response <-
        req
            R.GET
            ( https
                "gitlab.haskell.org"
                /: "api"
                /: "v4"
                /: "projects"
                /: showt projectId
                /: "jobs"
                /: showt jobId
            )
            NoReqBody
            jsonResponse
            (headerRedacted "PRIVATE-TOKEN" apiToken)
    pure (responseBody response)

--
-- Servant boilerplate
--

type WebHookAPI =
    ReqBody '[JSON] GitLabBuildEvent :> Post '[JSON] ()

webhookServer :: Token -> GitLabBuildEvent -> Handler ()
webhookServer apiToken glBuildEvent = do
    -- Fork a thread to do the processing and immediately return a 'success' status
    -- code to the caller; see the recommendations from GitLab documentation:
    -- https://docs.gitlab.com/ee/user/project/integrations/webhooks.html#configure-your-webhook-receiver-endpoint
    let fiveMin = 60 * 5
    pool <- liftIO $ createPool DB.connect DB.close 4 fiveMin 1
    let ctx = SpuriobotContext "" pool
    liftIO . void . forkIO $ (`runReaderT` ctx) $ runSpuriobot $ withTrace (showt (glbBuildId glBuildEvent)) $
        processBuildEvent
            apiToken
            glBuildEvent

webhookAPI :: Proxy WebHookAPI
webhookAPI = Proxy

webhookApplication :: ByteString -> Application
webhookApplication strApiToken =
    serve webhookAPI $ webhookServer strApiToken

--
-- Handler context setup
--

-- Version 0 of tracing is running handlers in a context where there's a logging
-- context we can use to decorate traces.

data SpuriobotContext = SpuriobotContext { traceContext :: Text, dbPool :: Pool Connection }

newtype Spuriobot a = Spuriobot { runSpuriobot :: ReaderT SpuriobotContext IO a }
    deriving newtype (Functor, Applicative, Monad, MonadReader SpuriobotContext, MonadIO)

withTrace :: Text -> Spuriobot a -> Spuriobot a
withTrace t_  = Spuriobot . withReaderT (modifyTraceContext (addContext t_)) . runSpuriobot
    where
        addContext t old_t
            | T.null old_t = t
            | otherwise = old_t <> ":" <> t
        modifyTraceContext f sc = sc { traceContext = f (traceContext sc) }

trace :: Text -> Spuriobot ()
trace t = withTrace t (liftIO . T.putStrLn =<< asks traceContext)

--
-- Controller logic
--

data SpuriobotException = ParseUrlFail
    deriving stock (Eq, Show)
    deriving anyclass Exception

-- | Get the raw job trace.
fetchJobLogs :: Token -> JobWebURL -> Spuriobot Text
fetchJobLogs apiToken jobWebURL = do
    let mbUri = do
            uri <- mkURI (jobWebURL <> "/raw")
            (uri', _) <- useHttpsURI uri
            pure uri'
    case mbUri of
        -- this error will only terminate the forked processJob thread, so the
        -- main process should keep listening and handling requests
        -- FIXME: use throwError in the Handler monad
        Nothing -> do
            trace $ "error: could not parse URL: " <> jobWebURL
            liftIO $ throwIO ParseUrlFail
        Just uri -> runReq defaultHttpConfig $ do
            -- TODO handle redirect to login which is gitlab's way of saying 404
            -- if re.search('users/sign_in$', resp.url):
            response <-
                req
                    R.GET
                    uri
                    NoReqBody
                    bsResponse
                    (headerRedacted "PRIVATE-TOKEN" apiToken)

            liftIO . pure . decodeUtf8 . responseBody $ response

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

-- writeFailuresToDB :: String -> String -> String -> [Failure] -> IO ()
-- writeFailuresToDB jobDate jobWebUrl jobRunnerId failures = do
--    conn <- connectPostgreSQLWithEnvArgs
--    void $
--        executeMany
--            conn
--            dbString
--            values
--  where
--    dbString :: Query
--    dbString =
--        mconcat
--            [ "insert into ci_failure (job_id, type, job_date, web_url, runner_id)"
--            , "values (?,?,?,?,?)"
--            , "on conflict do nothing"
--            ]
--    values = map (\(code, (jobId, _)) -> (jobId, code, jobDate, jobWebUrl, jobRunnerId)) failures

processBuildEvent :: Token -> GitLabBuildEvent -> Spuriobot ()
processBuildEvent apiToken GitLabBuildEvent{..} = do
    case glbBuildStatus of
        OtherBuildStatus x ->
            trace (x <> ":skipping")
        Failed -> withTrace "failed" $ processJob apiToken glbProjectId glbBuildId glbJobFailureReason

-- | Characteristics of a job that we test against.
data Jobbo = Jobbo (Maybe JobFailureReason) Text

processJob :: Token -> ProjectId -> JobId -> Maybe JobFailureReason -> Spuriobot ()
processJob apiToken glbProjectId glbBuildId glbJobFailureReason = do
    jobInfo <- fetchJobInfo apiToken glbProjectId glbBuildId
    logs <- fetchJobLogs apiToken (webUrl jobInfo)
    let jobbo = Jobbo glbJobFailureReason logs
    let failures = collectFailures jobbo
    logFailures failures

-- TODO keep a persistent connection around rather than making a new one each time
-- writeFailuresToDB glJobDate glJobWebURL glJobRunnerId failures
