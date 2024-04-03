{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

{- |
Module: Spuriobot
Description: Log spurious GHC GitLab build failures

Overall flow of the application:
- Receive a build event webhook call from GitLab
- Fetch the job log for that event
- Search in the log for known patterns for spurious failures
- Log the results to stdout
- TODO: write results to database
- TODO: tell the job to retry
-}
module Spuriobot (
    main,
) where

import Network.Wai.Handler.Warp (run)
import System.IO (hSetBuffering, stdout, BufferMode(..))
import System.Environment (
    getArgs,
    getEnv,
 )
import Control.Concurrent.Async (race_)
import Control.Concurrent.Classy (fork)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Servant
import Database.PostgreSQL.Simple (Connection)
import Data.Pool (Pool, newPool, defaultPoolConfig)
import Control.Monad (void)
import Control.Concurrent (newChan)


import qualified Spuriobot.DB as DB
import GitLabApi
import Spuriobot.RetryJob
import Spuriobot.Foundation
import Spuriobot.Spurio
import Control.Monad.Catch (finally)
import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (asks)

-- | API served by this app
type WebHookAPI =
    "spuriobot" :> ReqBody '[JSON] GitLabBuildEvent :> Post '[JSON] ()
    :<|> "spuriobot_system" :> ReqBody '[JSON] GitLabSystemEvent :> Post '[JSON] ()
    -- Backward compat top-level endpoint duplicating the "spuriobot" endpoint.
    :<|> ReqBody '[JSON] GitLabBuildEvent :> Post '[JSON] ()

webhookAPI :: Proxy WebHookAPI
webhookAPI = Proxy

main :: IO ()
main = do
    -- Ensure journald gets our output
    hSetBuffering stdout NoBuffering

    args <- getArgs
    case args of
        [] -> pure ()
        (_:_) -> error "Usage: spuriobot"

    strApiToken <- GitLabToken . encodeUtf8 . T.pack <$> getEnv "GITLAB_API_TOKEN"

    -- Die early if no DB connection. (Laziness in createPool bites us
    -- otherwise.)
    DB.close =<< DB.connect

    let fiveMin = 60 * 5
    pool <- newPool (defaultPoolConfig DB.connect DB.close fiveMin 1)

    chan <- RetryChan <$> newChan

    race_
        (runSpuriobot strApiToken pool chan retryService)
        (run 8080 $ logStdout $ serve webhookAPI (mainServer strApiToken pool chan))

spurioServer :: ServerT WebHookAPI Spuriobot
spurioServer = jobEvent :<|> systemEvent :<|> jobEvent
    where
        jobEvent = mkHook (showt . glbBuildId) processBuildEvent
        systemEvent = mkHook (const "system") processSystemEvent

mainServer :: GitLabToken -> Pool Connection -> RetryChan -> Server WebHookAPI
mainServer tok pool chan = hoistServer webhookAPI (runSpuriobot tok pool chan) spurioServer

-- | This turns the a request processor into a webhook endpoint by immediately
-- forking to do the real work.
--
-- See the recommendations from GitLab documentation:
-- https://docs.gitlab.com/ee/user/project/integrations/webhooks.html#configure-your-webhook-receiver-endpoint
mkHook
    :: (req -> TraceContext) -- ^ Generate a trace context from the request
    -> (req -> Spuriobot ()) -- ^ Process the request
    -> req
    -> Spuriobot ()
mkHook ctx fn req = void $ fork $ withTrace (ctx req) $ fn req

-- | Top-level handler for the GitLab job event
-- https://docs.gitlab.com/ee/user/project/integrations/webhook_events.html#job-events
processBuildEvent :: GitLabBuildEvent -> Spuriobot ()
processBuildEvent ev = do
    case glbFinishedAt ev of
        Nothing -> trace "skipping unfinished job"
        -- FIXME explain use of clearRetry here.
        Just _ -> withTrace "finished" $ processFinishedJob ev `finally` clearRetry (glbBuildId ev)

processFinishedJob :: GitLabBuildEvent -> Spuriobot ()
processFinishedJob ev = do
    case glbBuildStatus ev of
        OtherBuildStatus x -> trace x
        Failed -> withTrace "failed" $ processFailure ev

-- | Top-level handler for GitLab system events
-- https://gitlab.haskell.org/help/administration/system_hooks
processSystemEvent :: GitLabSystemEvent -> Spuriobot ()
processSystemEvent (ProjectSystemEvent ProjectCreate projId) =
    withTrace ("project " <> showt (unProjectId projId)) (installHook projId)
processSystemEvent (ProjectSystemEvent OtherProjectEvent projId) =
    withTrace ("project " <> showt (unProjectId projId)) (trace "skipping other project event")
processSystemEvent OtherSystemEvent = trace "skipping other system event"

installHook :: ProjectId -> Spuriobot ()
installHook projId = do
    tok <- asks apiToken
    liftIO $ addProjectBuildHook tok projId "http://127.0.0.1:8080/spuriobot"
    trace "hook installed"

--
-- Helpers
--

showt :: Show a => a -> Text
showt = T.pack . show
