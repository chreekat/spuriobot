{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- For MonadConc
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}

{-
 - Defines 'Spuriobot', the monad in which handlers run.
 -
 - Also defines some actions that run in the monad.
 -}
module Spuriobot.Foundation (
    SpuriobotContext(..),
    Spuriobot(..),
    withTrace,
    trace,
    TraceContext,
    runDB,
    runSpuriobot
) where

import Control.Concurrent.Classy (MonadConc)
import Control.Monad.Catch (MonadThrow, MonadMask, MonadCatch)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT(..), MonadReader, withReaderT, asks)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Database.PostgreSQL.Simple (Connection)
import Data.Pool (Pool, withResource)
import Control.Concurrent.STM (TMVar)

import {-# SOURCE #-} Spuriobot.RetryJob (RetryChan)
import GitLabApi (GitLabToken)
import qualified Database.SQLite.Simple as SQLite

-- Handler context (the Reader environment for the monad)
data SpuriobotContext = SpuriobotContext
    { apiToken :: GitLabToken
    , traceContext :: TraceContext
    , dbPool :: Pool Connection  -- PostgreSQL pool
    , retryChan :: RetryChan
    , connVar :: TMVar SQLite.Connection  -- SQLite connection
    }

-- Version 0 of tracing is running handlers in a context where there's a logging
-- context we can use to decorate traces.
type TraceContext = Text

-- The handler monad and its instances.
newtype Spuriobot a = Spuriobot { unSpuriobot :: ReaderT SpuriobotContext IO a }
    deriving newtype (Functor, Applicative, Monad, MonadReader SpuriobotContext, MonadIO, MonadThrow, MonadCatch, MonadMask, MonadConc)

-- Spuriobot action that adds to the trace context.
withTrace :: Text -> Spuriobot a -> Spuriobot a
withTrace t_  = Spuriobot . withReaderT (modifyTraceContext (addContext t_)) . unSpuriobot
    where
        addContext t old_t
            | T.null old_t = t
            | otherwise = old_t <> ":" <> t
        modifyTraceContext f sc = sc { traceContext = f (traceContext sc) }

-- Spuriobot action that prints a trace message in the current trace context.
-- Uses BS.putStr to prevent interleaving.
trace :: Text -> Spuriobot ()
trace t = withTrace t (liftIO . BS.putStr . encodeUtf8 . (<> "\n") =<< asks traceContext)

-- Spuriobot action that runs a database action for PostgreSQL.
runDB :: (Connection -> IO a) -> Spuriobot a
runDB db_act =
    let run_ pool = liftIO $ withResource pool db_act
    in withTrace "db" $ run_ =<< asks dbPool

-- Runner for 'Spuriobot' that initializes the Reader environment.
runSpuriobot :: GitLabToken -> Pool Database.PostgreSQL.Simple.Connection -> RetryChan -> TMVar SQLite.Connection -> Spuriobot a -> IO a
runSpuriobot tok pool chan connVar (Spuriobot act) = runReaderT act (SpuriobotContext tok "" pool chan connVar)