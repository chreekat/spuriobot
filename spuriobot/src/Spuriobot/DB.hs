{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | Module for all db queries. 
module Spuriobot.DB (connect, Database.PostgreSQL.Simple.close, insertFailures, Failure(..)) where

import GHC.Generics
import Data.Foldable (toList)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Int (Int64)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple hiding (connect)

type FailType = Text

data Failure = Failure Int64 FailType UTCTime Text (Maybe Int64) (Maybe Text) Text Text
    deriving (Eq, Show, Generic, ToRow)

type DB a = Connection -> IO a

-- | Connect using only configuration from the environment.
connect :: IO Connection
connect = connectPostgreSQL ""

-- | Insert a batch of failures.
insertFailures :: Foldable f => f Failure -> DB Int64
insertFailures x conn = executeMany conn
    [sql|
        insert into ci_failure (job_id, type, job_date, web_url, runner_id, runner_name, job_name, project_path)
        values (?, ?, ?, ?, ?, ?, ?, ?)
        on conflict do nothing
        |]
    (toList x)
