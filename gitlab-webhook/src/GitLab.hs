{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module GitLab (
    GitLabBuildEvent (..),
    grepForFailures,
    webhookApplication,
    webhookAPI,
) where

import Control.Concurrent (forkIO)
import Control.Monad (
    forM_,
    void,
 )
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (
    FromJSON,
    ToJSON,
    object,
    parseJSON,
    toJSON,
    withObject,
    (.:),
    (.=),
 )
import Data.ByteString (ByteString)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.IO as T
import Network.HTTP.Req (
    NoReqBody (..),
    bsResponse,
    defaultHttpConfig,
    headerRedacted,
    req,
    responseBody,
    runReq,
    useHttpsURI,
 )
import qualified Network.HTTP.Req as R
import Servant (
    Application,
    JSON,
    Post,
    Proxy (..),
    ReqBody,
    Server,
    serve,
    (:>),
 )
import Text.Regex.TDFA (
    (=~),
 )
import Text.URI (mkURI)
import TextShow (showt)

import qualified GitLabApi

-- overall flow of the application:
--
-- receive a webhook call
-- fetch the job log
-- grep for patterns in the log
-- write results to database
-- TODO: tell the job to retry
--
-- we want to:
--
-- replicate existing behaviour
-- with tests
-- and a systemd unit
-- deployable via nix

type WebHookAPI =
    ReqBody '[JSON] GitLabBuildEvent :> Post '[JSON] ()

webhookServer :: GitLabApi.Token -> Server WebHookAPI
webhookServer apiToken glBuildEvent = do
    -- TODO proper logging
    -- Fork a thread to do the processing and immediately return a 'success' status
    -- code to the caller; see the recommendations from GitLab documentation:
    -- https://docs.gitlab.com/ee/user/project/integrations/webhooks.html#configure-your-webhook-receiver-endpoint
    liftIO . void . forkIO $
        processJob
            apiToken
            glBuildEvent

webhookAPI :: Proxy WebHookAPI
webhookAPI = Proxy

webhookApplication :: ByteString -> Application
webhookApplication strApiToken =
    serve webhookAPI $ webhookServer strApiToken

-- BuildEvent is what the webhook receives
data GitLabBuildEvent = GitLabBuildEvent
    { glbRef :: String
    , glbBuildId :: Int
    , glbBuildName :: String
    , glbBuildStatus :: String
    , glbBuildFailureReason :: String
    , glbProjectId :: GitLabApi.ProjectId
    }
    deriving (Show, Ord, Eq)

instance FromJSON GitLabBuildEvent where
    parseJSON = withObject "GitLabBuildEvent" $ \v ->
        GitLabBuildEvent
            <$> v .: "ref"
            <*> v .: "build_id"
            <*> v .: "build_name"
            <*> v .: "build_status"
            <*> v .: "build_failure_reason"
            <*> v .: "project_id"

instance ToJSON GitLabBuildEvent where
    toJSON x =
        object
            [ "ref" .= glbRef x
            , "build_id" .= glbBuildId x
            , "build_name" .= glbBuildName x
            , "build_status" .= glbBuildStatus x
            , "build_failure_reason" .= glbBuildFailureReason x
            , "project_id" .= glbProjectId x
            ]

fetchJobLogs :: GitLabApi.Token -> GitLabApi.JobWebURL -> IO Text
fetchJobLogs apiToken jobWebURL = do
    let mbUri = do
            uri <- mkURI (jobWebURL <> "/raw")
            (uri', _) <- useHttpsURI uri
            pure uri'
    case mbUri of
        -- this error will only terminate the forked processJob thread, so the
        -- main process should keep listening and handling requests
        Nothing -> error $ "Could not parse URL: " <> T.unpack jobWebURL
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

-- TODO tests
grepForFailures :: Text -> Set Failure
grepForFailures =
    S.fromList . concatMap searchErrors . T.lines
  where
    searchErrors :: Text -> [Failure]
    searchErrors s = mapMaybe (findError s) regexes
    findError :: Text -> (Text, (FailureMessage, FailureErrorCode)) -> Maybe Failure
    findError line (regex, (msg, code)) = if line =~ regex then Just (code, msg) else Nothing
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
            , ("GitLab connection failure", "connect_gitlab")
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
logFailures :: GitLabApi.JobId -> Set Failure -> IO ()
logFailures jobId failures =
    forM_
        (S.toList failures)
        ( \(_, msg) ->
            T.putStrLn $ "job " <> showt jobId <> ": " <> msg
        )

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

processJob :: GitLabApi.Token -> GitLabBuildEvent -> IO ()
processJob apiToken GitLabBuildEvent{..} = do
    jobInfo <- GitLabApi.fetchJobInfo apiToken glbProjectId glbBuildId
    logs <- fetchJobLogs apiToken (GitLabApi.webUrl jobInfo)
    let failures = grepForFailures logs
    logFailures glbBuildId failures

-- TODO keep a persistent connection around rather than making a new one each time
-- writeFailuresToDB glJobDate glJobWebURL glJobRunnerId failures
