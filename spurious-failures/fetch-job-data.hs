{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

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
import Network.HTTP.Req
import System.Environment
import System.IO
import Text.URI
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T

import Debug.Trace

-- For each project
--   - Look up the most-recent fetched job
--   - Start fetching jobs until we don't see new ones and/or we've fetched a
--     job older than a month old.
--   - Store the job info we care about.
--       - (Or just store the entire json blob! Why not!)
--   - Fetch the trace for the job
--
-- Fetching traces depends on fetching job ids and an http session. Fetching job
-- ids depends on querying the most-recent fetched job. There are a lot of
-- independent leaf operations. Some day I could parallelize them. PCPH Ch. 13
-- (p. 225).
--

data Project = Project { name :: String, projectId :: Int } deriving (Eq, Show)

-- | List of projects we care about.
-- Some day we might make this group-wide.
projects :: [Project]
projects =
    [ Project "ghc/ghc" 1
    -- For debugging
    -- , Project "ghc/head.hackage" 78
    -- , Project "ghc/ghc-debug" 798
    -- , Project "haskell/haskell-language-server" 1180
    -- , Project "ghc/ci-images" 149
    ]

-- | The elements of a GitLab REST API Job entity we care about
data Job = Job
    { jobId :: Int
    , jobBlob :: Text
    , createdAt :: UTCTime
    , webUrl :: Text
    } deriving (Eq, Show)

instance ToRow Job where
    toRow (Job j blob _ _) = toRow (j, blob)

instance FromRow Job where
    fromRow = Job <$> field <*> field <*> field <*> field

v .:: key = \subkey -> do
    subobj <- v .: (fromString key)
    withObject key (.: subkey) subobj

instance FromJSON Job where
    parseJSON = withObject "Job" $ \v -> Job
        <$> v .: "id"
        <*> pure (T.decodeUtf8 (toStrict (encode v)))
        <*> v .: "created_at"
        <*> v .: "web_url"

jobsAPI :: Project -> URI
jobsAPI (projectId -> i) = fromJust $ mkURI $
    "https://gitlab.haskell.org/api/v4/projects/"
    <> T.pack (show i)
    <> "/jobs?scope%5B%5D=failed&scope%5b%5d=success&per_page=100"

data JobsResult
    = WithMore URI [Job]
    | NoMore [Job]
    deriving (Eq, Show)


-- | A quick and partial action that adds a PRIVATE-TOKEN header with the key
api uri key =
    let Just (u, o) = useHttpsURI uri
    in reqq GET u NoReqBody jsonResponse (o <> header "PRIVATE-TOKEN" key)

-- | Get a list of jobs in a single query. Includes info about whether we should
-- continue.
queryJobs key minAge jobUrl connVar = do
    resp <- api jobUrl key

    nextLink <- head <$> responseLinks "rel" "next" resp
    let jobs = responseBody resp

    -- Check if we're still in the age range
    let inAgeRange j = createdAt j > minAge

    -- And that we are still seeing new jobs
    [Only seenCount] <- bracketDB "new jobs?" connVar $ \conn -> do
        execute_ conn "create temp table if not exists fetched_jobs (job_id primary key) without rowid"
        execute_ conn "delete from fetched_jobs"
        executeMany conn "insert into fetched_jobs (job_id) values (?)" (map (Only . jobId) jobs)
        query_ conn "select count(*) from job j join fetched_jobs f on f.job_id = j.job_id"
    let stillNewJobs = length jobs > seenCount

    pure $ if all inAgeRange jobs && stillNewJobs
        then WithMore nextLink jobs
        else NoMore jobs

-- | Get all jobs later than a given age.
getJobs key minAge jobUrl connVar = do
    logg $ "Get " <> (T.encodeUtf8 (render jobUrl))
    res <- lift $ queryJobs key minAge jobUrl connVar
    case res of
        NoMore jobs -> pure jobs
        WithMore nextUrl jobs -> pure jobs <|> getJobs key minAge nextUrl connVar

data Trace = Trace
    { tid :: Int
    , trace :: Text
    } deriving (Eq, Show)

instance ToJSON Trace where
    toJSON (Trace i t) = object [ "id" .= i, "trace" .= t ]

instance ToRow Trace where
    toRow (Trace j l) = toRow (j, l)


-- | The simple api we all we wished for
reqq method url body resp opts = liftIO $ runReq defaultHttpConfig (req method url body resp opts)

-- | Get the trace for a job
getTrace key j = do
    logg $ "GET TRACE " <> (bstr (show (jobId j)))
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
        execute conn "insert into job_trace (rowid, trace) values (?, ?)" t

logg :: MonadIO m => BS.ByteString -> m ()
logg = liftIO . BS.hPutStrLn stderr

-- | Make atomic db access via atomic access to the Connection.
bracketDB msg v
    = liftIO
    . bracket (do c <- atomically (takeTMVar v); logg ("OPEN " <> msg); pure c) (\c -> logg ("CLOSE " <> msg) >> (atomically (putTMVar v c)))

-- | Concurrently insert all staged jobs.
clearStagedJobs key connVar = do
    logg "Clearing staged jobs"
    jobs <- bracketDB "jobs with no traces" connVar
        $ \conn -> query_ conn [sql|
            select j.job_id, j.json, j.created_at, j.web_url
            from job j
            left join job_trace t
            on j.job_id = t.rowid
            where t.rowid is null
        |]
    logg ("CLEAR " <> (bstr (show (length jobs))) <> " JOBS")
    parMapM (insertJob key connVar) jobs

-- | Fetch jobs and dump them in the job table
stageJobs key connVar lastMonth projURL = do
    logg "Staging jobs"
    runListT $ do
        j <- getJobs key dateRange projURL connVar
        case j of
            [] -> pure ()
            _ -> bracketDB "insert jobs" connVar $ \conn -> executeMany conn "insert into job values (?,?) on conflict do nothing" j

initDatabase connVar = do
    bracketDB "init database" connVar $ \conn -> execute_ conn [sql|
        create table if not exists job (
            job_id int primary key,
            json text not null,
            created_at text generated always as (json ->> '$.created_at'),
            web_url text generated always as (json ->> '$.web_url')
        )
        without rowid;
        create virtual table if not exists job_trace using fts5(trace, content='');
    |]

main = do
    connVar <- atomically . newTMVar =<< open "jobs.db"
    bracketDB "set pragmas" connVar $ \conn -> do
        execute_ conn "PRAGMA mmap_size=6442450944" -- (6 GiB)
        execute_ conn "PRAGMA journal_mode=wal"

    now <- getCurrentTime
    let lastMonth = addUTCTime (-30 * nominalDay) now
    args <- getArgs
    let cutoffDate = fromMaybe lastMonth $
            case args of
                [userDate] -> iso8601ParseM userDate
                _ -> Nothing


    key <- BS.pack <$> getEnv "GIT_PRIVATE_TOKEN"

    initDatabase connVar

    forM_ projects $ \proj -> do
        stageJobs key connVar cutoffDate (jobsAPI proj)
    runParIO (clearStagedJobs key connVar)
