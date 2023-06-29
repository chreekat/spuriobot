{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | Module for all db queries. 
module Spuriobot.DB (insertFailures) where

import GHC.Generics
import Data.Foldable (toList)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Int (Int64)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple

type FailType = Text

data Failure = Failure Int64 FailType UTCTime Text Int64
    deriving (Eq, Show, Generic, ToRow)

-- | Insert a batch of failures.
insertFailures :: Foldable f => Connection -> f Failure -> IO Int64
insertFailures conn x = executeMany conn
    [sql|
        insert into ci_failure (job_id, type, job_date, web_url, runner_id)
        values (?, ?, ?, ?, ?)
        on conflict do nothing
        |]
    (toList x)
