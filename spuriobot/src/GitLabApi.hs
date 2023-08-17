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
    ProjectId (..),
    JobId,
    JobWebUrl,
    fetchJobInfo,
    JobInfo(..),
    JobFailureReason(..),
    BuildStatus(..),
    retryJobApi,
    RetryResult(..),
    JobWebUrlParseFailure(..),
    fetchJobLogs,
    GitLabSystemEvent(..),
    ProjectEventType(..),
) where

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
import Data.Text (Text)
import Network.HTTP.Req (
    NoReqBody (..),
    defaultHttpConfig,
    headerRedacted,
    https,
    jsonResponse,
    req,
    responseBody,
    runReq,
    (/:), (/~), useHttpsURI, bsResponse,
 )
import qualified Network.HTTP.Req as R
import Data.Time (UTCTime)
import Data.Int (Int64)
import Data.Time.LocalTime (localTimeToUTC, utc)

import GHC.Generics (Generic)
import Data.ByteString (ByteString)
import Text.URI (mkURI)
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
type JobWebUrl = Text

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
data JobInfo = JobInfo
    { webUrl :: JobWebUrl
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
fetchJobInfo :: GitLabToken -> ProjectId -> JobId -> IO JobInfo
fetchJobInfo (GitLabToken tok) (ProjectId projectId) jobId = do
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

newtype JobWebUrlParseFailure = JobWebUrlParseFailure Text

-- | Get the raw job trace. The JobWebUrl may fail to parse.
fetchJobLogs :: GitLabToken -> JobWebUrl -> IO (Either JobWebUrlParseFailure Text)
fetchJobLogs (GitLabToken tok) jobWebURL = do
    let url = jobWebURL <> "/raw"
        mbUri = do
            uri <- mkURI url
            (uri', _) <- useHttpsURI uri
            pure uri'
    case mbUri of
        Nothing -> pure (Left (JobWebUrlParseFailure url))
        Just uri -> do
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
