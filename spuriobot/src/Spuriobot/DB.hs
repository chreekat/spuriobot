{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | Module for all db queries.
module Spuriobot.DB (
    connect,
    Database.PostgreSQL.Simple.close,
    insertFailures,
    Failure(..),
    connectSQLite, -- Exported the SQLite connection function
    insertJobs,
    insertJobTrace,
) where

import GHC.Generics
import Data.Foldable (toList)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Int (Int64)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple hiding (connect, executeMany)
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.SQLite.Simple as SQLite
import Control.Concurrent.STM (TMVar)

import Spuriobot.Backfill (bracketDB,Trace(..),JobWithProjectPath(..))

type FailType = Text

data Failure = Failure Int64 FailType UTCTime Text (Maybe Int64) (Maybe Text) Text Text
    deriving (Eq, Show, Generic, PG.ToRow)

type DB a = PG.Connection -> IO a

-- | Connect using only configuration from the environment.
connect :: IO PG.Connection
connect = connectPostgreSQL ""

-- | Connect to SQLite using the given database file.
connectSQLite :: String -> IO SQLite.Connection
connectSQLite = SQLite.open

-- | Insert a batch of failures.
insertFailures :: Foldable f => f Failure -> DB Int64
insertFailures x conn = PG.executeMany conn
    [sql|
        insert into ci_failure (job_id, type, job_date, web_url, runner_id, runner_name, job_name, project_path)
        values (?, ?, ?, ?, ?, ?, ?, ?)
        on conflict do nothing
        |]
    (toList x)

-- | Function to insert batch of jobs to SQLite FTS database
insertJobs :: Foldable f => f JobWithProjectPath -> TMVar SQLite.Connection -> IO ()
insertJobs x connVar =
    bracketDB "insert jobs" connVar $ \conn ->
        SQLite.executeMany conn "insert into job (job_id, job_date, web_url, runner_id, runner_name, job_name, project_path) values (?,?,?,?,?,?,?)" (toList x)

-- | Function to insert trace of job logs to FTS database
insertJobTrace :: Foldable f => f Trace -> TMVar SQLite.Connection -> IO ()
insertJobTrace traces connVar = do
    let tracesToInsert = map (\(Trace a b) -> (a, b)) (toList traces)
    bracketDB "insert job traces" connVar $ \conn ->
        SQLite.executeMany conn "insert into job_trace (job_id, logs) values (?,?)" tracesToInsert
