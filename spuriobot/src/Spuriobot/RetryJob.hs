{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- Used for MonadConc:
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Spuriobot.RetryJob (
    RetryResult(..),
    RetryAction(..),
    RetryCmd(..),
    RetryChan(..),
    clearRetry,
    retryJob,
    retryService,
) where

import Control.Monad.Catch (try)
import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (asks)
import Data.Aeson (
    FromJSON,
    parseJSON,
    withObject,
    (.:),
 )
import qualified Data.Map as M
import qualified Data.Text as T
import Network.HTTP.Req (
    NoReqBody (..),
    defaultHttpConfig,
    headerRedacted,
    https,
    jsonResponse,
    req,
    responseBody,
    runReq,
    (/:), (/~), HttpException,
 )
import qualified Network.HTTP.Req as R
import Data.Int (Int64)
import Data.Text (Text)
import Control.Exception (displayException)
import Control.Concurrent (Chan, writeChan, readChan)

import Data.Void (Void)
import Control.Retry (retrying, limitRetries, fullJitterBackoff)

import GitLabApi
import Spuriobot.Foundation

showt :: Show a => a -> Text
showt = T.pack . show


--
-- Retry handling
--

data RetryChan = RetryChan { rchan :: Chan RetryAction }
type RetryMap = M.Map JobId Word

data RetryAction = RetryAction JobId TraceContext RetryCmd
data RetryCmd
    = Retry ProjectId
    -- ^ One needs to know the project id for a job to send a retry to GitLab
    | Clear

clearRetry :: JobId -> Spuriobot ()
clearRetry jobId = do
    (RetryChan chan) <- asks retryChan
    ctx <- asks traceContext
    liftIO $ writeChan chan (RetryAction jobId ctx Clear)

retryJob :: ProjectId -> JobId -> Spuriobot ()
retryJob projId jobId = do
    (RetryChan chan) <- asks retryChan
    ctx <- asks traceContext
    liftIO $ writeChan chan (RetryAction jobId ctx (Retry projId))

-- | This is an action that continually pulls from the chan and handles
-- 'RetryCmd' commands.
--
-- This "service" will keep running even if GitLab requests experience HTTP
-- failures. Other kinds of exceptions, however, will crash the thread, which
-- will bubble up and crashes the whole app. systemd will need to take over in
-- that case.
retryService :: Spuriobot Void
retryService = loop M.empty where
    -- Number of times we will retry a job.
    maxRetries = 10

    loop :: RetryMap -> Spuriobot Void
    loop retryMap = do
        RetryAction jobId ctx cmd <- liftIO . readChan . rchan =<< asks retryChan
        -- A job that hasn't been retried isn't in the map, so we return 0
        let retryCount = M.findWithDefault 0 jobId retryMap
        -- We're done with this entry now, so delete it. Note this makes 'Clear'
        -- a no-op in handle_cmd.
        let retryMap' = M.delete jobId retryMap
        loop =<< withTrace ctx (handle_cmd jobId retryMap' retryCount cmd)

    handle_cmd jobId retryMap retryCount cmd = do
        case cmd of
            Clear -> pure retryMap
            Retry projId ->
                if retryCount < maxRetries
                then do
                    newId' <- retry_job projId jobId
                    case newId' of
                        Just newId -> do
                            trace $ "retried as job: " <> showt newId
                            pure (M.insert newId (retryCount + 1) retryMap)
                        Nothing -> pure retryMap
                else do
                    trace "exceeded max retries"
                    pure retryMap

    retry_job :: ProjectId -> JobId -> Spuriobot (Maybe JobId)
    retry_job (ProjectId projectId) jobId =
        let -- Test if we should retry the retry action.
            test_exception (Left e) = do
                withTrace "WARN" $ do
                    trace "Retry failed."
                    mapM_ trace (T.lines . T.pack . displayException $ e)
                pure True
            test_exception (Right _) = pure False


            -- This sends the retry command to GitLab.
            -- The type signature is crucial as it constrains `try`.
            retry_action :: Spuriobot (Either HttpException JobId)
            retry_action = try $ do
                tok <- asks apiToken
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

            policy = limitRetries 5 <> fullJitterBackoff halfSecond
                where halfSecond = 500 * 1000
        in do
            res <- retrying policy (const test_exception) (const retry_action)
            case res of
                Left _ -> Nothing <$ trace ("ERR: Giving up retrying job " <> showt jobId)
                Right j -> pure $ Just j

newtype RetryResult = RetryResult { retryJobId :: Int64 }
    deriving (Eq, Show)

instance FromJSON RetryResult where
    parseJSON = withObject "RetryResult" $ \o ->
        RetryResult <$> o .: "id"
