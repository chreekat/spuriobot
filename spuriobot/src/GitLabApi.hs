{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- Used for MonadConc:
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module GitLabApi (
    GitLabBuildEvent (..),
    GitLabToken(..),
    GitLabTime(..),
    ProjectId (..),
    JobId,
    fetchFinishedJob,
    fetchProject,
    FinishedJob(..),
    Project(..),
    JobFailureReason(..),
    BuildStatus(..),
    retryJobApi,
    RetryResult(..),
    JobWebUrlParseFailure(..),
    fetchJobLogs,
    GitLabSystemEvent(..),
    ProjectEventType(..),
    JobWebhook(..),
    addProjectBuildHook,
    JobWebURI(..)
) where

import Data.Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Attoparsec.Text as AttoText
import qualified Data.Attoparsec.Time as Atto
import Data.Text (Text)
import Network.HTTP.Req (
    NoReqBody (..),
    ReqBodyJson (..),
    defaultHttpConfig,
    headerRedacted,
    https,
    jsonResponse,
    req,
    responseBody,
    runReq,
    (/:), (/~), bsResponse, ignoreResponse, useURI,
 )
import qualified Network.HTTP.Req as R
import Data.Time (UTCTime)
import Data.Int (Int64)
import Data.Time.LocalTime (localTimeToUTC, utc)

import GHC.Generics (Generic)
import Data.ByteString (ByteString)
import Text.URI (mkURI, URI)
import Data.Text.Encoding (decodeUtf8)
import Control.Applicative ((<|>))

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

newtype ProjectId = ProjectId {unProjectId :: Int}
    deriving stock (Show, Ord, Eq)
    deriving newtype (FromJSON, ToJSON)

-- TODO convert to newtype when the fancy strikes
type JobId = Int64

----------------------
-- * /job API endpoint
----------------------




-- | The data we get from the /job API endpoint for finished jobs.
--
-- Most of what we need could come from the BuildEvent, but the web_url in
-- particular is missing, so it's not sufficient for our use.
--
-- Informally, we use BuildEvent to decide whether or not to check the job for
-- failures, and the /job endpoint for everything else.
--
-- That the job is actually finished is not checked. However, by assuming it
-- *is* finished, we know that jobFinishedAt is guaranteed to exist.
data FinishedJob = FinishedJob
    { webUrl :: JobWebURI
    , runnerId :: Maybe Int64
    , runnerName :: Maybe Text
    -- ^ GitLab can "lose" runner info, so runner fields are 'Maybe'
    , jobFinishedAt :: UTCTime
    , jobFailureReason :: Maybe JobFailureReason
    , jobName :: Text
    }
    deriving (Show, Eq)

instance FromJSON FinishedJob where
    parseJSON = withObject "FinishedJob" $ \o ->
        FinishedJob
            <$> o .: "web_url"
            <*> (o .:? "runner" >>= maybe (pure Nothing) (.: "id"))
            <*> (o .:? "runner" >>= maybe (pure Nothing) (.: "description"))
            <*> o .: "finished_at"
            <*> o .:? "failure_reason"
            <*> o .: "name"

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




--------------------------
-- * /project API endpoint
--------------------------


-- | Data from the /project API endpoint. Just used to store as metadata when
-- recording failures.
newtype Project = Project
    { projPath :: Text
    } deriving (Eq, Show)

instance FromJSON Project where
    parseJSON = withObject "Project" $ \o ->
        Project
            <$> o .: "path_with_namespace"

fetchProject :: GitLabToken -> ProjectId -> IO Project
fetchProject (GitLabToken tok) (ProjectId projectId) = do
    fmap responseBody $ runReq defaultHttpConfig $
        req
            R.GET
            ( https
                "gitlab.haskell.org"
                /: "api"
                /: "v4"
                /: "projects"
                /~ projectId
            )
            NoReqBody
            jsonResponse
            (headerRedacted "PRIVATE-TOKEN" tok)



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

-- | Get /jobs/<job-id>
fetchFinishedJob :: GitLabToken -> ProjectId -> JobId -> IO FinishedJob
fetchFinishedJob (GitLabToken tok) (ProjectId projectId) jobId = do
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
retryJobApi (GitLabToken tok) (ProjectId projectId) jobId =
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


----------------------
-- * /job/<job-id>/raw
----------------------

newtype JobWebUrlParseFailure = JobWebUrlParseFailure URI

newtype JobWebURI = JobWebURI URI
    deriving (Eq, Show)

instance FromJSON JobWebURI where
    parseJSON = withText "JobWebURI" $ \v -> do
        maybe (fail "could not parse URI") (pure . JobWebURI) (mkURI v)

-- | Get the raw job trace. The JobWebUrl may fail to parse.
--
-- This function uses the web UI rather than the actual GitLab JSON API. That's
-- because the trace endpoint of the API is very slow.
fetchJobLogs :: GitLabToken -> JobWebURI -> IO (Either JobWebUrlParseFailure Text)
fetchJobLogs (GitLabToken tok) (JobWebURI jobURI) =
    let url = useURI jobURI
    in case url of
        Just (Left (url', opt)) -> fetch_job_raw url' opt
        Just (Right (url', opt)) -> fetch_job_raw url' opt
        Nothing -> pure (Left (JobWebUrlParseFailure jobURI))

    where
        fetch_job_raw url opt =
            let raw_url = url /: "raw"
            in runReq defaultHttpConfig $ do
                -- TODO handle redirect to login which is gitlab's way of saying 404
                -- if re.search('users/sign_in$', resp.url):
                response <-
                    req
                        R.GET
                        raw_url
                        NoReqBody
                        bsResponse
                        (opt <> headerRedacted "PRIVATE-TOKEN" tok)

                pure . Right . decodeUtf8 . responseBody $ response


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

-- FIXME: If the url has 'https', set enable_ssl_verification to true.
--
-- NB: push_events defaults to True
instance ToJSON JobWebhook where
    toJSON (JobWebhook url) = object
        [ "url" .= url
        , "job_events" .= True
        , "push_events" .= False
        , "enable_ssl_verification" .= False
        ]
    toEncoding (JobWebhook url) = pairs
        ("url" .= url
        <> "job_events" .= True
        <> "push_events" .= False
        <> "enable_ssl_verification" .= False
        )

-- | Add a build webhook to a project
addProjectBuildHook :: GitLabToken -> ProjectId -> Text -> IO ()
addProjectBuildHook (GitLabToken tok) (ProjectId projId) hook =
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
                /~ projId
                /: "hooks"
