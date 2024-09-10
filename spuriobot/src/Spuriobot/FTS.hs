{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Module for interacting with the FTS database.
module Spuriobot.FTS (
    Job(Job),
    Trace(..),
    bracketDB,
    bracketDB2,
    fetchJobsBetweenDates,
    initDatabase,
    insertLogtoFTS,
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
      GET(GET),
      NoReqBody(NoReqBody) )
import qualified Network.HTTP.Req as Req
import Data.Proxy (Proxy)

import GitLabApi (Job(..), Project(..), ProjectId(..), JobWebURI(..), FinishedJob (..))
import Control.Monad.Catch (MonadThrow)
import Spuriobot.Foundation (Spuriobot, connVar)
import Control.Monad.Reader (asks)

-- | List of projects we care about.
projects :: [Project]
projects =
    [ Project "ghc/ghc" (ProjectId 1)
    ]

-- Loose coupling wrapper.
data FTSJob = FTSJob
    { ftsJobId :: Int64
    , ftsJobUrl :: Text
    -- ^ Used for fetching the job trace.
    --
    -- I don't bother with 'URI' because req's interface requires 'useHttpsURI',
    -- so I have to accept potential failure no matter what.
    , ftsJobObject :: Text
    } deriving stock (Eq, Show)

instance ToRow FTSJob where
    toRow (FTSJob {..}) = toRow (ftsJobId, ftsJobUrl, ftsJobObject)

instance FromRow FTSJob where
    fromRow = FTSJob <$> field <*> field <*> field

jobToFTSJob :: Job -> FTSJob
jobToFTSJob Job {..} = FTSJob jobId (render (getJobWebURI webUrl)) jobBlob

finishedJobToFTSJob :: FinishedJob -> FTSJob
finishedJobToFTSJob FinishedJob {..} = FTSJob finishedJobId (render (getJobWebURI finishedJobWebUrl)) finishedJobBlob

-- | Builds a URI to the jobs endpoint from a Project and an optional page
-- number.
jobsAPI :: Project -> Maybe Int -> URI
jobsAPI (projId -> i) page =
    let u = "https://gitlab.haskell.org/api/v4/projects/"
            <> T.pack (show (unProjectId i))
            <> "/jobs?scope%5B%5D=failed&scope%5b%5d=success&per_page=100"
            <> maybe "" (\p -> "&page=" <> T.pack (show p)) page
        v = mkURI u
    in case v of
        Left e -> error $ "Bad URI parse: " <> show e
        Right v' -> v'

-- | This deals with pagination. It returns the jobs fetched, but also encodes
-- whether or not there still more left to fetch.
--
-- On review, 'TooYoung u' has the same data and effect as 'WithMore u []', and
-- could theoretically be dropped. But that could be confusing, so I'm keeping
-- it.
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
        tooYoung j = jobCreatedAt j > maxDate
        tooOld j = jobCreatedAt j < minDate
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
getTrace :: MonadIO m =>  BS.ByteString -> (Int64, Text) -> m Trace
getTrace key (jobId', jobUrl') = do
    logg $ "GET TRACE " <> bstr (show jobId')
    let (u,o) = fromMaybe (error "Bad URI parse") (useHttpsURI =<< mkURI jobUrl')
    resp <- reqq GET (u /: "raw") NoReqBody bsResponse (o <> header "PRIVATE-TOKEN" key)
    pure $ Trace jobId' (T.decodeUtf8 $ responseBody resp)

bstr :: String -> BS.ByteString
bstr = T.encodeUtf8 . T.pack

-- | Fetch and store a job trace. Ignore duplicates.
fetchAndInsertTrace :: MonadIO m => BS.ByteString -> TMVar Connection -> (Int64, Text) -> m ()
fetchAndInsertTrace key connVar job = do
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

-- | A new bracketDB that catches errors thrown by SQLite itself (as opposed to
-- the sqlite-simple library).
bracketDB2 :: MonadIO m => BS.ByteString -> TMVar conn -> (conn -> IO res) -> m (Either SQLError res)
bracketDB2 msg v = liftIO . try . bracketDB msg v

-- | Concurrently insert all staged jobs.
clearStagedJobs :: BS.ByteString -> TMVar Connection -> ParIO ()
clearStagedJobs key connVar = do
    logg "Clearing staged jobs"
    jobs <- bracketDB "jobs with no traces" connVar
        $ \conn -> query_ conn [sql|
            select j.job_id, j.job_url
            from job j
            left join job_trace t
            on j.job_id = t.rowid
            where t.rowid is null
        |]
    logg ("CLEAR " <> bstr (show (length jobs)) <> " JOBS")
    void $ parMapM (fetchAndInsertTrace key connVar) jobs

-- | Fetch jobs and dump them in the job table
stageJobs :: BS.ByteString -> TMVar Connection -> (UTCTime, UTCTime) -> URI -> IO ()
stageJobs key connVar dateRange projURL = do
    logg "Staging jobs"
    runListT $ do
        j <- getJobs key dateRange projURL connVar
        bracketDB "insert jobs" connVar $ \conn ->
            executeMany conn jobInsertString (fmap jobToFTSJob j)

jobInsertString :: Query
jobInsertString = "insert or ignore into job (job_id, job_url, job_blob) values (?, ?, ?)"

-- | Initialize the database
initDatabase :: MonadIO m => TMVar Connection -> m ()
initDatabase connVar = do
    bracketDB "init database" connVar $ \conn -> do
        execute_ conn [sql|
            create table if not exists job (
                job_id int primary key,
                job_url text not null,
                job_blob text not null
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

-- | This inserts the job metadata to job table in sql and log trace in job_trace table setting up FTS database
insertLogtoFTS :: FinishedJob -> Spuriobot ()
insertLogtoFTS f@FinishedJob { .. } = do
    sqliteconnVar <- asks connVar
    void $ liftIO $ bracketDB "insert job" sqliteconnVar $ \conn -> do
        execute conn jobInsertString (finishedJobToFTSJob f)
        execute conn "insert into job_trace (rowid, trace) values (?, ?)" (finishedJobId, finishedJobLogs)
