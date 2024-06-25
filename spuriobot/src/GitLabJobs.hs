{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module GitLabJobs (
    fetchJobsBetweenDates,
    initDatabase,
    bracketDB,
    Trace(..)
) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.Par.IO
import Control.Monad.Par.Combinator
import Data.Aeson
import Data.ByteString.Lazy (toStrict)
import Data.Function
import Data.Maybe
import Data.String (fromString)
import Data.Text (Text)
import Data.Time
import Data.Time.Format.ISO8601
import Database.SQLite.Simple
import Database.SQLite.Simple.QQ
import List.Transformer as List
import Network.HTTP.Client hiding (responseBody)
import Network.HTTP.Client.TLS
import System.Environment
import System.IO
import Text.URI
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import GHC.Generics (Generic)
import Network.HTTP.Req
    ( (/:),
      bsResponse,
      defaultHttpConfig,
      header,
      jsonResponse,
      req,
      responseBody,
      responseLinks,
      runReq,
      useHttpsURI,
      https,
      (/~),
      headerRedacted,
      GET(GET),
      NoReqBody(NoReqBody) )
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Aeson.Types (FromJSON, parseEither)


data Project = Project { name :: String, projectId :: Int } deriving (Eq, Show)

projects :: [Project]
projects =
    [ Project "ghc/ghc" 1
    ]

data Job = Job
    { jobId        :: Int
    , createdAt    :: UTCTime
    , webUrl       :: Text
    , runnerId     :: Maybe Int
    , runnerName   :: Maybe Text
    , jobName      :: Maybe Text
    , glbProjectId :: Maybe Int
    } deriving (Eq, Show)

instance ToRow Job where
    toRow (Job jobId createdAt webUrl runnerId runnerName jobName glbProjectId) =
        toRow (jobId, createdAt, webUrl, runnerId, runnerName, jobName, glbProjectId)

instance FromRow Job where
    fromRow = Job <$> field <*> field <*> field <*> field <*> field <*> field <*> field

instance FromJSON Job where
    parseJSON = withObject "Job" $ \v -> do
        jobId <- v .: "id"
        createdAt <- v .: "created_at"
        webUrl <- v .: "web_url"
        runner <- v .:? "runner"
        runnerId <- traverse (.: "id") runner
        runnerName <- traverse (.: "name") runner
        jobName <- v .:? "name"
        pipeline <- v .:? "pipeline"
        glbProjectId <- traverse (.: "project_id") pipeline
        return Job
            { jobId = jobId
            , createdAt = createdAt
            , webUrl = webUrl
            , runnerId = runnerId
            , runnerName = runnerName
            , jobName = jobName
            , glbProjectId = glbProjectId
            }

jobsAPI :: Project -> Maybe Int -> URI
jobsAPI (projectId -> i) page = fromJust $ mkURI $
    "https://gitlab.haskell.org/api/v4/projects/"
    <> T.pack (show i)
    <> "/jobs?scope%5B%5D=failed&scope%5b%5d=success&per_page=100"
    <> maybe "" (\p -> "&page=" <> T.pack (show p)) page

data JobsResult
    = WithMore URI [Job]
    | NoMore [Job]
    | TooYoung URI
    deriving (Eq, Show)

api uri key =
    let Just (u, o) = useHttpsURI uri
    in reqq GET u NoReqBody jsonResponse (o <> header "PRIVATE-TOKEN" key)

fetchJobs key (minDate, maxDate) jobUrl connVar = do
    resp <- api jobUrl key

    nextLink <- head <$> responseLinks "rel" "next" resp
    let jobs = responseBody resp

    let (msg, res) = f jobs nextLink
        tooYoung j = createdAt j > maxDate
        tooOld j = createdAt j < minDate
        f jobs link | all tooYoung jobs = ("Too young", TooYoung link)
                    | all tooOld jobs = ("Too old", NoMore jobs)
                    | otherwise = ("Found jobs", WithMore link jobs)
    logg msg
    pure res

getJobs
    :: BS.ByteString
    -> (UTCTime, UTCTime)
    -> URI
    -> t
    -> ListT IO [Job]
getJobs key dateRange jobUrl connVar = do
    logg $ "Get " <> T.encodeUtf8 (render jobUrl)
    res <- lift $ fetchJobs key dateRange jobUrl connVar
    case res of
        NoMore jobs -> pure jobs
        WithMore nextUrl jobs -> pure jobs List.<|> getJobs key dateRange nextUrl connVar
        TooYoung nextUrl -> getJobs key dateRange nextUrl connVar

data Trace = Trace
    { tid :: Int
    , trace :: Text
    } deriving (Eq, Show)

instance ToJSON Trace where
    toJSON (Trace i t) = object [ "id" .= i, "trace" .= t ]

instance ToRow Trace where
    toRow (Trace j l) = toRow (j, l)

reqq method url body resp opts = liftIO $ runReq defaultHttpConfig (req method url body resp opts)

getTrace key j = do
    logg $ "GET TRACE " <> bstr (show (jobId j))
    let Just (u,o) = useHttpsURI =<< mkURI (webUrl j)
    resp <- reqq GET (u /: "raw") NoReqBody bsResponse (o <> header "PRIVATE-TOKEN" key)
    pure $ Trace
        (jobId j)
        (T.decodeUtf8 $ responseBody resp)

bstr = T.encodeUtf8 . T.pack

-- | Move a staged job into the actual job table. Fetch and store its trace as
-- well. Ignore duplicates.
insertJob key connVar job = do
    t <- getTrace key job
    bracketDB "insert trace" connVar $ \conn ->
        execute conn "insert into job_trace (rowid, trace) values (?, ?) on conflict do nothing" t

logg :: MonadIO m => BS.ByteString -> m ()
logg = liftIO . BS.hPutStrLn stderr

-- | Make atomic db access via atomic access to the Connection.
bracketDB msg v
    = liftIO
    . bracket (do c <- atomically (takeTMVar v); logg ("OPEN " <> msg); pure c) (\c -> logg ("CLOSE " <> msg) >> atomically (putTMVar v c))

newtype GitLabToken = GitLabToken BS.ByteString

data ProjectInfo = ProjectInfo
    { project_path :: T.Text
    } deriving (Eq, Show)

instance FromJSON ProjectInfo where
    parseJSON = withObject "ProjectInfo" $ \v -> ProjectInfo
        <$> v .: "path"

fetchProject :: GitLabToken -> Int -> IO ProjectInfo
fetchProject (GitLabToken tok) projectId = do
    resp <- runReq defaultHttpConfig $ do
        req GET
            (https "gitlab.haskell.org" /: "api" /: "v4" /: "projects" /~ projectId)
            NoReqBody
            jsonResponse
            (header "PRIVATE-TOKEN" tok)
    case parseEither parseJSON (responseBody resp) of
        Left err -> error $ "Failed to decode project JSON: " ++ show err
        Right proj -> return proj

instance ToRow JobWithProjectPath where
    toRow (JobWithProjectPath jobId createdAt webUrl runnerId runnerName jobName projectPath) =
        toRow (jobId, createdAt, webUrl, runnerId, runnerName, jobName, projectPath)

-- | Concurrently insert all staged jobs.
clearStagedJobs key connVar = do
    logg "Clearing staged jobs"
    jobs <- bracketDB "jobs with no traces" connVar
        $ \conn -> query_ conn [sql|
            select j.job_id, j.job_date, j.web_url, j.runner_id, j.runner_name, j.job_name, j.project_path
            from job j
            left join job_trace t
            on j.job_id = t.rowid
            where t.rowid is null
        |]
    logg ("CLEAR " <> bstr (show (length jobs)) <> " JOBS")
    void $ parMapM (insertJob key connVar) jobs

data JobWithProjectPath = JobWithProjectPath
    { jobIdjwpp        :: Int
    , createdAtjwpp    :: UTCTime
    , webUrljwpp       :: Text
    , runnerIdjwpp     :: Maybe Int
    , runnerNamejwpp   :: Maybe Text
    , jobNamejwpp      :: Maybe Text
    , project_pathjwpp :: Text
    } deriving (Eq, Show)

-- | Fetch jobs and dump them in the job table
stageJobs key connVar dateRange projURL = do
    logg "Staging jobs"
    runListT $ do
        j <- getJobs key dateRange projURL connVar
        case j of
            [] -> pure ()
            jobs -> do
                forM_ jobs $ \job -> do
                    projectInfo <- liftIO $ fetchProject (GitLabToken key) (fromMaybe 0 (glbProjectId job))
                    let jobWithProjectPath = JobWithProjectPath
                                { jobIdjwpp = jobId job
                                , createdAtjwpp = createdAt job
                                , webUrljwpp = webUrl job
                                , runnerIdjwpp = runnerId job
                                , runnerNamejwpp = runnerName job
                                , jobNamejwpp = jobName job
                                , project_pathjwpp = project_path projectInfo
                                }
                    bracketDB "insert jobs" connVar $ \conn ->
                        execute conn "insert or ignore into job (job_id, job_date, web_url, runner_id, runner_name, job_name, project_path) values (?,?,?,?,?,?,?)" jobWithProjectPath

-- | Initialize the database

initDatabase connVar = do
    bracketDB "init database" connVar $ \conn -> do
        execute_ conn [sql|
            create table if not exists job (
                job_id int primary key,
                job_date text,
                web_url text,
                runner_id int,
                runner_name text,
                job_name text,
                project_path text
            )
            without rowid;
        |]
        execute_ conn [sql|
            create virtual table if not exists job_trace using fts5(trace, content='');
        |]

-- | Fetch jobs between given dates
fetchJobsBetweenDates :: (UTCTime, UTCTime) -> IO ()
fetchJobsBetweenDates dateRange = do
    connVar <- newTMVarIO =<< open "jobs.db"
    key <- BS.pack <$> getEnv "GIT_PRIVATE_TOKEN"
    initDatabase connVar
    forM_ projects $ \proj -> do
        stageJobs key connVar dateRange (jobsAPI proj Nothing)
    runParIO (clearStagedJobs key connVar)