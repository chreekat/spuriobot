{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

-- | Module for backfilling FTS database for all job logs till that point.
module Spuriobot.Backfill (
    fetchJobsBetweenDates,
    initDatabase,
    bracketDB,
    Trace(..),
    JobWithProjectPath(..),
    Job(Job),
) where

import Data.Int (Int64)
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.Par.IO
import Control.Monad.Par.Combinator
import Data.Aeson
import Data.Maybe
import Data.Text (Text)
import Data.Time
import Database.SQLite.Simple
import Database.SQLite.Simple.QQ
import List.Transformer as List
import System.Environment
import System.IO
import Text.URI
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Req
    ( (/:),
      bsResponse,
      defaultHttpConfig,
      header,
      jsonResponse,
      JsonResponse,
      req,
      responseBody,
      responseLinks,
      runReq,
      useHttpsURI,
      https,
      (/~),
    --   headerRedacted,
      GET(GET),
      NoReqBody(NoReqBody) )
import qualified Network.HTTP.Req as Req
import Data.Aeson.Types (parseEither)
import Control.Monad.Catch (MonadThrow)
import Data.Proxy (Proxy)


data Project = Project { name :: String, projectId :: Int } deriving (Eq, Show)

-- | List of projects we care about.
projects :: [Project]
projects =
    [ Project "ghc/ghc" 1
    ]

-- | The elements of a GitLab REST API Job entity we care about
-- FIXME: We care about a ton of stuff, actually, so this should be greatly
-- expanded.
data Job = Job
    { jobId        :: Int64
    , createdAt    :: UTCTime
    , webUrl       :: Text
    , runnerId     :: Maybe Int64
    , runnerName   :: Maybe Text
    , jobName      :: Text
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
        runnerId <- v .:? "runner" >>= maybe (pure Nothing) (.: "id")
        runnerName <-  v .:? "runner" >>= maybe (pure Nothing) (.: "description")
        jobName <- v .: "name"
        pipeline <- v .:? "pipeline"
        glbProjectId <- traverse (.: "project_id") pipeline
        return Job {..}

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

-- | A quick and partial action that adds a PRIVATE-TOKEN header with the key
api :: (MonadIO m, FromJSON a) => URI -> BS.ByteString -> m (Network.HTTP.Req.JsonResponse a)
api uri key =
    let (u, o) = fromMaybe (error "Bad URI parse") (useHttpsURI uri)
    in reqq GET u NoReqBody jsonResponse (o <> header "PRIVATE-TOKEN" key)

-- | Get a list of jobs in a single query. Includes info about whether we should
-- continue.
fetchJobs :: (MonadIO m, MonadThrow m) => BS.ByteString -> (UTCTime, UTCTime) -> URI -> m JobsResult
fetchJobs key (minDate, maxDate) jobUrl = do
    resp <- api jobUrl key

    nextLink <- head <$> responseLinks "rel" "next" resp
    let jobs' = responseBody resp

    let (msg, res) = f jobs' nextLink
        -- Check if we're still in the age range
        tooYoung j = createdAt j > maxDate
        tooOld j = createdAt j < minDate
        f jobs link | all tooYoung jobs = ("Too young", TooYoung link)
                    | all tooOld jobs = ("Too old", NoMore jobs)
                    | otherwise = ("Found jobs", WithMore link jobs)
    logg msg
    pure res

-- | Get all jobs withen a given age range.
getJobs
    :: BS.ByteString
    -> (UTCTime, UTCTime)
    -> URI
    -> t
    -> ListT IO [Job]
getJobs key dateRange jobUrl connVar = do
    logg $ "Get " <> T.encodeUtf8 (render jobUrl)
    res <- lift $ fetchJobs key dateRange jobUrl
    case res of
        NoMore jobs -> pure jobs
        WithMore nextUrl jobs -> pure jobs List.<|> getJobs key dateRange nextUrl connVar
        TooYoung nextUrl -> getJobs key dateRange nextUrl connVar

data Trace = Trace
    { tid :: Int64
    , trace :: Text
    } deriving (Eq, Show)

instance ToJSON Trace where
    toJSON (Trace i t) = object [ "id" .= i, "trace" .= t ]

instance ToRow Trace where
    toRow (Trace j l) = toRow (j, l)

-- | The simple api we all we wished for
reqq
    :: (Req.HttpBodyAllowed    (Req.AllowsBody method) (Req.ProvidesBody body), MonadIO m, Req.HttpMethod method, Req.HttpBody body,  Req.HttpResponse a)
    => method -> Req.Url scheme -> body -> Proxy a -> Req.Option scheme -> m a
reqq method url body resp opts = liftIO $ runReq defaultHttpConfig (req method url body resp opts)

-- | Get the trace for a job
getTrace :: MonadIO m =>  BS.ByteString -> Job -> m Trace
getTrace key j = do
    logg $ "GET TRACE " <> bstr (show (jobId j))
    let (u,o) = fromMaybe (error "Bad URI parse") (useHttpsURI =<< mkURI (webUrl j))
    resp <- reqq GET (u /: "raw") NoReqBody bsResponse (o <> header "PRIVATE-TOKEN" key)
    pure $ Trace
        (jobId j)
        (T.decodeUtf8 $ responseBody resp)

bstr :: String -> BS.ByteString
bstr = T.encodeUtf8 . T.pack

-- | Move a staged job into the actual job table. Fetch and store its trace as
-- well. Ignore duplicates.
insertJob :: MonadIO m => BS.ByteString -> TMVar Connection -> Job -> m ()
insertJob key connVar job = do
    t <- getTrace key job
    bracketDB "insert trace" connVar $ \conn ->
        execute conn "insert into job_trace (rowid, trace) values (?, ?)" t

logg :: MonadIO m => BS.ByteString -> m ()
logg = liftIO . BS.hPutStrLn stderr

-- | Make atomic db access via atomic access to the Connection.
bracketDB :: MonadIO m => BS.ByteString -> TMVar a1 -> (a1 -> IO a2) -> m a2
bracketDB msg v
    = liftIO
    . bracket (do c <- atomically (takeTMVar v); logg ("OPEN " <> msg); pure c) (\c -> logg ("CLOSE " <> msg) >> atomically (putTMVar v c))

newtype GitLabToken = GitLabToken BS.ByteString

newtype ProjectInfo = ProjectInfo
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
    toRow (JobWithProjectPath j p) = toRow (j :. Only p)

instance FromRow JobWithProjectPath where
    fromRow = do
        j :. Only p <- fromRow
        pure $ JobWithProjectPath j p

-- | Concurrently insert all staged jobs.
clearStagedJobs :: BS.ByteString -> TMVar Connection -> ParIO ()
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

data JobWithProjectPath = JobWithProjectPath Job Text
    deriving (Eq, Show)

-- | Fetch jobs and dump them in the job table
stageJobs :: BS.ByteString -> TMVar Connection -> (UTCTime, UTCTime) -> URI -> IO ()
stageJobs key connVar dateRange projURL = do
    logg "Staging jobs"
    runListT $ do
        j <- getJobs key dateRange projURL connVar
        case j of
            [] -> pure ()
            jobs -> do
                forM_ jobs $ \job -> do
                    -- FIXME: We already have the project url, so we should
                    -- already have the project path as well.
                    projectInfo <- liftIO $ fetchProject (GitLabToken key) (fromMaybe 0 (glbProjectId job))
                    let jobWithProjectPath = JobWithProjectPath job (project_path projectInfo)
                    bracketDB "insert jobs" connVar $ \conn ->
                        execute conn "insert or ignore into job (job_id, job_date, web_url, runner_id, runner_name, job_name, project_path) values (?,?,?,?,?,?,?)" jobWithProjectPath

-- | Initialize the database
initDatabase :: MonadIO m => TMVar Connection -> m ()
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

