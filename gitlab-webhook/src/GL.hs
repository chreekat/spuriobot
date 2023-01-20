{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module GL (
    webhookApplication,
    postGLBuildEvent,
    GLBuildEvent (..),
) where

-- FIXME constrain imports

import Control.Concurrent (forkIO)
import Control.Monad (
    forM_,
    void,
 )
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Maybe (mapMaybe)
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import Database.PostgreSQL.Simple
import Network.HTTP.Req as R
import Network.Wai
import Servant
import Servant.Client (ClientM, client)
import Text.Regex.TDFA

-- overall flow of the application?
--
-- receive a webhook call
-- fetch the job log
-- grep for patterns in the log
-- write results to database
-- (maybe tell the job to retry? ask bryan)
--
--
-- we want to:
--   replicate existing behaviour
--   with tests
--   and a systemd unit
--   deployable via nix

-- TODO: add this back in
newtype GitlabEventHeader = GitlabEventHeader Text deriving (Eq, Show)

type WebHookAPI =
    -- FIXME stupid formolu formatting
    ReqBody '[JSON] GLBuildEvent
        --        :> Header "Gitlab-Event" GitlabEventHeader
        :> Post '[JSON] ()

webhookServer :: ByteString -> Server WebHookAPI
webhookServer connString glBuildEvent = do
    liftIO $ print glBuildEvent
    liftIO $ void $ forkIO (processJob connString (jobInfoFromBuildEvent glBuildEvent))

jobInfoFromBuildEvent :: GLBuildEvent -> GLJobInfo
jobInfoFromBuildEvent GLBuildEvent{..} =
    GLJobInfo
        { glJobId = show glbBuildId
        , glJobDate = glbCreated
        , glJobWebURL = undefined
        , glJobRunnerId = undefined
        }

-- webhookServer glBuildEvent gitlabEventHeader = do
--    liftIO $ print glBuildEvent >> print gitlabEventHeader

webhookAPI :: Proxy WebHookAPI
webhookAPI = Proxy

webhookApplication :: ByteString -> Application
webhookApplication = serve webhookAPI . webhookServer

-- for testing the webhook
-- This isn't a great test, because we are only sending a subset of the fields
-- over. However it does test that the webhook server works at all.
postGLBuildEvent :: GLBuildEvent -> ClientM ()
postGLBuildEvent = client webhookAPI

data GLCommit = GLCommit
    { glcId :: Int
    , glcSha :: String
    , glcMessage :: String
    , glcAuthorName :: String
    }
    deriving (Show, Ord, Eq)

instance FromJSON GLCommit where
    parseJSON = withObject "GLCommit" $ \v ->
        GLCommit
            <$> v
            .: "id"
            <*> v
            .: "sha"
            <*> v
            .: "message"
            <*> v
            .: "author_name"

instance ToJSON GLCommit where
    toJSON x =
        object
            [ "id" .= glcId x
            , "sha" .= glcSha x
            , "message" .= glcMessage x
            , "author_name" .= glcAuthorName x
            ]

-- FIXME what's the difference between a "Build" and a "Job"?
data GLBuildEvent = GLBuildEvent
    { glbRef :: String
    , glbBuildId :: Int
    , glbBuildName :: String
    , glbBuildStatus :: String -- FIXME
    , glbBuildFailureReason :: String
    , glbProjectId :: Int
    , glbCommit :: GLCommit
    , glbCreated :: String
    }
    deriving (Show, Ord, Eq)

-- FIXME fourmolu too opinionated here
instance FromJSON GLBuildEvent where
    parseJSON = withObject "GLBuildEvent" $ \v ->
        GLBuildEvent
            <$> v
            .: "ref"
            <*> v
            .: "build_id"
            <*> v
            .: "build_name"
            <*> v
            .: "build_status"
            <*> v
            .: "build_failure_reason"
            <*> v
            .: "project_id"
            <*> v
            .: "commit"
            <*> v
            .: "build_created_at"

instance ToJSON GLBuildEvent where
    toJSON x =
        object
            [ "ref" .= glbRef x
            , "build_id" .= glbBuildId x
            , "build_name" .= glbBuildName x
            , "build_status" .= glbBuildStatus x
            , "build_failure_reason" .= glbBuildFailureReason x
            , "project_id" .= glbProjectId x
            , "commit" .= glbCommit x
            , "build_created_at" .= glbCreated x
            ]

fetchJobLogs :: String -> IO String
fetchJobLogs glJobWebUrl = runReq defaultHttpConfig $ do
    -- TODO handle redirect to login which is gitlab's way of saying 404
    -- if re.search('users/sign_in$', resp.url):
    r <- req R.GET (https "gitlab.haskell.org" /: (T.pack . show $ glJobWebUrl)) NoReqBody jsonResponse mempty
    pure $ show (r :: JsonResponse ()) -- FIXME placeholder type

-- the code that we inject into the database
type FailureErrorCode = String

type GLJobId = String

-- the message we echo to stdout
type FailureMessage = String

type JobFailureMessage = (GLJobId, FailureMessage)

type Failure = (FailureErrorCode, JobFailureMessage)

-- TODO tests
grepForFailures :: GLJobId -> [String] -> [Failure]
grepForFailures jobId =
    concatMap f
  where
    f :: String -> [Failure]
    f s = mapMaybe (g s) regexes
    g :: String -> (String, (FailureMessage, FailureErrorCode)) -> Maybe Failure
    g line (regex, (msg, code)) = if line =~ regex then Just (code, (jobId, msg)) else Nothing
    regexes =
        [
            ( "Cannot connect to the Docker daemon at unix:///var/run/docker.sock"
            , ("docker failure", "docker")
            )
        ,
            ( "failed to pull image \"registry.gitlab.haskell.org"
            , ("image pull failure", "pull_image")
            )
        ,
            ( "Failed to connect to gitlab.haskell.org"
            , ("GitLab connection failure", "gitlab_connect")
            )
        ,
            ( "No space left on device"
            , ("exhausted disk", "no_space")
            )
        ,
            ( "failed due to signal 9 .Killed"
            , ("received signal 9", "signal_9")
            )
        ,
            ( "Idle CPU consumption too different"
            , ("T16916 failed", "T16916")
            )
        ,
            ( "Cannot allocate memory|osCommitMemory: VirtualAlloc MEM_COMMIT failed|out of memory allocating \\d+ bytes"
            , ("could not allocate memory", "cannot_allocate")
            )
        ,
            ( "MoveFileEx"
            , ("MoveFileEx-related failure", "MoveFileEx")
            )
        ]

-- TODO real logging
logFailures :: [Failure] -> IO ()
logFailures failures =
    forM_
        failures
        ( \(_, (jobId, msg)) ->
            putStrLn $ "job " <> jobId <> ": " <> msg
        )

writeFailuresToDB :: ByteString -> String -> String -> String -> [Failure] -> IO ()
writeFailuresToDB connString jobDate jobWebUrl jobRunnerId failures = do
    conn <- connectPostgreSQL connString
    void $
        executeMany
            conn
            dbString
            values
  where
    dbString :: Query
    dbString =
        mconcat
            [ "insert into ci_failure (job_id, type, job_date, web_url, runner_id)"
            , "values (?,?,?,?,?)"
            , "on conflict do nothing"
            ]
    values = map (\(code, (jobId, _)) -> (jobId, code, jobDate, jobWebUrl, jobRunnerId)) failures

data GLJobInfo = GLJobInfo
    { glJobId :: GLJobId
    , glJobDate :: String
    , glJobWebURL :: String
    , glJobRunnerId :: String
    }
    deriving (Show, Ord, Eq)

processJob :: ByteString -> GLJobInfo -> IO ()
processJob connString GLJobInfo{..} = do
    logs <- fetchJobLogs glJobId
    let failures = grepForFailures glJobId . lines $ logs
    logFailures failures
    -- TODO keep a persistent connection around rather than making a new one each time
    writeFailuresToDB connString glJobDate glJobWebURL glJobRunnerId failures
