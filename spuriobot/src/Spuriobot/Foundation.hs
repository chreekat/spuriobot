{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- For MonadConc
{-# LANGUAGE UndecidableInstances #-}

module Spuriobot.Foundation (SpuriobotContext(..), Spuriobot(..), withTrace, trace, TraceContext, runDB, runSpuriobot) where

import Control.Concurrent.Classy (MonadConc)
import Control.Monad.Catch (MonadThrow, MonadMask, MonadCatch)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT(..), MonadReader, withReaderT, asks)
import Data.ByteString (ByteString, )
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Database.PostgreSQL.Simple (Connection)
import Data.Pool (Pool, withResource)

import {-# SOURCE #-} Spuriobot.RetryJob (RetryChan)

--
-- Handler context setup
--

-- Version 0 of tracing is running handlers in a context where there's a logging
-- context we can use to decorate traces.

data SpuriobotContext = SpuriobotContext
    { apiToken :: ByteString
    , traceContext :: TraceContext
    , dbPool :: Pool Connection
    , retryChan :: RetryChan
    }

type TraceContext = Text

newtype Spuriobot a = Spuriobot { unSpuriobot :: ReaderT SpuriobotContext IO a }
    deriving newtype (Functor, Applicative, Monad, MonadReader SpuriobotContext, MonadIO, MonadThrow, MonadCatch, MonadMask, MonadConc)

-- | Spuriobot combinator that adds to the trace context.
withTrace :: Text -> Spuriobot a -> Spuriobot a
withTrace t_  = Spuriobot . withReaderT (modifyTraceContext (addContext t_)) . unSpuriobot
    where
        addContext t old_t
            | T.null old_t = t
            | otherwise = old_t <> ":" <> t
        modifyTraceContext f sc = sc { traceContext = f (traceContext sc) }

-- | Spuriobot action that prints a trace message in the current trace context.
-- Uses BS.putStr to prevent interleaving.
trace :: Text -> Spuriobot ()
trace t = withTrace t (liftIO . BS.putStr . encodeUtf8 . (<> "\n") =<< asks traceContext)

-- | Spuriobot action that runs a database action.
runDB :: (Connection -> IO a) -> Spuriobot a
runDB db_act =
    let run_ pool = liftIO $ withResource pool db_act
    in withTrace "db" $ run_ =<< asks dbPool

runSpuriobot :: MonadIO m => ByteString -> Pool Connection -> RetryChan -> Spuriobot a -> m a
runSpuriobot tok pool chan (Spuriobot act) = liftIO $ runReaderT act (SpuriobotContext tok "" pool chan)
