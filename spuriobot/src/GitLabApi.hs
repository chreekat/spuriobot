{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- Used for MonadConc:
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module GitLabApi (
    GitLabBuildEvent (..),
    GitLabToken(..),
    GitLabTime(..),
    ProjectId (..),
    JobId,
    Job(..),
    FinishedJob(..),
    Project(..),
    JobFailureReason(..),
    BuildStatus(..),
    retryJobApi,
    RetryResult(..),
    JobWebUrlParseFailure(..),
    GitLabSystemEvent(..),
    ProjectEventType(..),
    JobWebhook(..),
    addProjectBuildHook,
    JobWebURI(..),
    gitlab,
    finishedJobToJob
) where

import Control.Applicative ((<|>))
import Control.Monad.Catch
import Data.Aeson
import Data.Aeson.Encoding (text)
import Data.Aeson.Types qualified as Aeson
import Data.Attoparsec.Text qualified as AttoText
import Data.Attoparsec.Time qualified as Atto
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Time (UTCTime)
import Data.Time.LocalTime (localTimeToUTC, utc)
import GHC.Generics (Generic)
import Network.HTTP.Req (
    (/:),
    (/~),
    NoReqBody (..),
    ReqBodyJson (..),
    defaultHttpConfig,
    headerRedacted,
    https,
    ignoreResponse,
    jsonResponse,
    req,
    responseBody,
    runReq,
    )
import Network.HTTP.Req qualified as R
import Text.URI (mkURI, URI)

--
-- GitLab API types and handlers
--

newtype GitLabToken = GitLabToken ByteString

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

newtype ProjectId = ProjectId {unProjectId :: Int64}
    deriving stock (Show, Ord, Eq)
    deriving newtype (FromJSON, ToJSON)

-- TODO convert to newtype when the fancy strikes
type JobId = Int64

----------------------
-- * /job API endpoint
----------------------




-- | The data we get from the /job API endpoint.
--
-- Most of what we need could come from the BuildEvent, but the web_url in
-- particular is missing, so it's not sufficient for our use.
--
-- Informally, we use BuildEvent to decide whether or not to check the job for
-- failures, and the /job endpoint for everything else.
data Job = Job
    { jobId :: JobId
    , webUrl :: JobWebURI
    , runnerId :: Maybe Int64
    , runnerName :: Maybe Text
    -- ^ GitLab can "lose" runner info, so runner fields are 'Maybe'
    , jobCreatedAt :: UTCTime
    , jobFinishedAt :: Maybe UTCTime
    , jobFailureReason :: Maybe JobFailureReason
    , jobName :: Text
    , jobBlob :: Text
    -- ^ The whole JSON object provided by GitLab
    }
    deriving (Show, Eq)

instance FromJSON Job where
    parseJSON = withObject "Job" $ \o ->
        Job
            <$> o .: "id"
            <*> o .: "web_url"
            <*> (o .:? "runner" >>= maybe (pure Nothing) (.: "id"))
            <*> (o .:? "runner" >>= maybe (pure Nothing) (.: "description"))
            <*> o .: "created_at"
            <*> o .:? "finished_at"
            <*> o .:? "failure_reason"
            <*> o .: "name"
            <*> pure (T.decodeUtf8 (BS.toStrict (encode o)))

-- | Failure reasons that we care about.
data JobFailureReason = JobTimeout | JobStuck | RunnerSystemFailure | OtherReason Text
    deriving (Eq, Show)

instance FromJSON JobFailureReason where
    parseJSON = withText "JobFailureReason" (pure . f) where
        f "job_execution_timeout" = JobTimeout
        f "stuck_or_timeout_failure" = JobStuck
        f "runner_system_failure" = RunnerSystemFailure
        f x = OtherReason x

-- | Used in the ToField instance
instance ToJSON JobFailureReason where
    toJSON JobTimeout = "job_execution_timeout"
    toJSON JobStuck = "stuck_or_timeout_failure"
    toJSON RunnerSystemFailure = "runner_system_failure"
    toJSON (OtherReason x) = toJSON x

    toEncoding JobTimeout = text "job_execution_timeout"
    toEncoding JobStuck = text "stuck_or_timeout_failure"
    toEncoding RunnerSystemFailure = text "runner_system_failure"
    toEncoding (OtherReason x) = text x

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



-- | This is a sort of god-object for processing of finished jobs. It mostly
-- mirrors Job (which comes from the GitLab API), except we know jobFinishedAt
-- is non-null, and we add logs and the missing data that comes from the Project API
-- endpoint.
data FinishedJob = FinishedJob
    { finishedJobWebUrl :: JobWebURI
    , finishedJobRunnerId :: Maybe Int64
    , finishedJobRunnerName :: Maybe Text
    -- ^ GitLab can "lose" runner info, so runner fields are 'Maybe'
    , finishedJobFinishedAt :: UTCTime
    , finishedJobFailureReason :: Maybe JobFailureReason
    , finishedJobName :: Text
    , finishedJobProjectPath :: Text
    , finishedJobProjectId :: ProjectId
    , finishedJobLogs :: Text
    , finishedJobId :: JobId
    , finishedJobCreatedAt :: UTCTime
    , finishedJobBlob :: Text
    -- ^ The whole JSON object provided by GitLab
    }
    deriving (Show, Eq)

-- convert back to a job
finishedJobToJob :: FinishedJob -> Job
finishedJobToJob FinishedJob {..} = Job
    { jobId = finishedJobId
    , webUrl = finishedJobWebUrl
    , runnerId = finishedJobRunnerId
    , runnerName = finishedJobRunnerName
    , jobCreatedAt = finishedJobCreatedAt
    , jobFinishedAt = Just finishedJobFinishedAt
    , jobFailureReason = finishedJobFailureReason
    , jobName = finishedJobName
    , jobBlob = finishedJobBlob
    }

--------------------------
-- * /project API endpoint
--------------------------


-- | Data from the /project API endpoint. Just used to store as metadata when
-- recording failures. And for that matter, it's just used to store the project
-- path. (GitlabBuildEvent has something called project_name, but that comes out
-- as "Glasgow Haskell Compiler / head.hackage".
data Project = Project
    { projPath :: Text
    , projId :: ProjectId
    } deriving (Eq, Show)

instance FromJSON Project where
    parseJSON = withObject "Project" $ \o ->
        Project
            <$> o .: "path_with_namespace"
            <*> o .: "id"

-----------------------------
-- * Job webhook
-----------------------------



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


------------------------
-- * /job/<job-id>/retry
------------------------


newtype RetryResult = RetryResult { retryJobId :: Int64 }
    deriving (Eq, Show)

instance FromJSON RetryResult where
    parseJSON = withObject "RetryResult" $ \o ->
        RetryResult <$> o .: "id"

-- | This name is dumb, but even worse would be to have the clashing names
-- Spuriobot.RetryJob.retryJob and GitLabApi.retryJob. Sorry.
retryJobApi :: GitLabToken -> ProjectId -> JobId -> IO JobId
retryJobApi (GitLabToken tok) (ProjectId projectId) job =
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
                /~ job
                /: "retry"


----------------------
-- * /job/<job-id>/raw
----------------------

newtype JobWebUrlParseFailure = JobWebUrlParseFailure URI deriving (Show)

instance Exception JobWebUrlParseFailure

newtype JobWebURI = JobWebURI { getJobWebURI :: URI }
    deriving (Eq, Show)

instance FromJSON JobWebURI where
    parseJSON = withText "JobWebURI" $ \v -> do
        maybe (fail "could not parse URI") (pure . JobWebURI) (mkURI v)

-------------------
-- * System webhook
-------------------


data ProjectEventType = ProjectCreate | OtherProjectEvent
    deriving stock (Show, Eq, Generic)

instance FromJSON ProjectEventType where
    parseJSON = withText "ProjectEventType" parseType
        where
            parseType "project_create" = pure ProjectCreate
            parseType _ = pure OtherProjectEvent

data GitLabSystemEvent
    = ProjectSystemEvent ProjectEventType ProjectId
    | OtherSystemEvent
    deriving stock (Show, Eq, Generic)

instance FromJSON GitLabSystemEvent where
    parseJSON p = parseProject p <|> pure OtherSystemEvent
        where
            parseProject = withObject "GitLabSystemEvent" $ \v ->
                ProjectSystemEvent
                    <$> v .: "event_name"
                    <*> v .: "project_id"

newtype JobWebhook = JobWebhook Text
    deriving (Eq, Show)

-- | NB: push_events defaults to True, though this isn't documented.
instance ToJSON JobWebhook where
    toJSON (JobWebhook url) = object
        [ "url" .= url
        , "job_events" .= True
        , "push_events" .= False
        , "enable_ssl_verification" .= "https://" `T.isPrefixOf` url
        ]
    toEncoding (JobWebhook url) = pairs
        ("url" .= url
        <> "job_events" .= True
        <> "push_events" .= False
        <> "enable_ssl_verification" .= "https://" `T.isPrefixOf` url
        )

-- | Add a build webhook to a project
addProjectBuildHook :: GitLabToken -> ProjectId -> Text -> IO ()
addProjectBuildHook (GitLabToken tok) (ProjectId proj) hook =
    fmap responseBody $ runReq defaultHttpConfig $
        req
            R.POST
            projectHookUrl
            body
            ignoreResponse
            (headerRedacted "PRIVATE-TOKEN" tok)
    where
        body = ReqBodyJson (JobWebhook hook)
        projectHookUrl =
            https "gitlab.haskell.org"
                /: "api"
                /: "v4"
                /: "projects"
                /~ proj
                /: "hooks"

gitlab :: R.Url 'R.Https
gitlab = R.https "gitlab.haskell.org" /: "api" /: "v4"
