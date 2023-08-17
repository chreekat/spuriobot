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
import Servant (
    JSON,
    Post,
    Proxy (..),
    ReqBody,
    Server,
    hoistServer,
    serve,
    (:>), ServerT,
 )
import Database.PostgreSQL.Simple (Connection)
import Data.Pool (Pool, createPool)
import Control.Monad (void)
import Control.Concurrent (newChan)


import qualified Spuriobot.DB as DB
import GitLabApi
import Spuriobot.RetryJob
import Spuriobot.Foundation
import Spuriobot.Spurio

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
    pool <- createPool DB.connect DB.close 4 fiveMin 1

    chan <- RetryChan <$> newChan

    race_
        (runSpuriobot strApiToken pool chan retryService)
        (run 8080 $ logStdout $ serve webhookAPI (mainServer strApiToken pool chan))

spurioServer :: ServerT WebHookAPI Spuriobot
spurioServer = jobEvent

mainServer :: GitLabToken -> Pool Connection -> RetryChan -> Server WebHookAPI
mainServer tok pool chan = hoistServer webhookAPI (runSpuriobot tok pool chan) spurioServer

-- | This turns the job processor into a webhook endpoint by immediately forking
-- to do the real work.
--
-- See the recommendations from GitLab documentation:
-- https://docs.gitlab.com/ee/user/project/integrations/webhooks.html#configure-your-webhook-receiver-endpoint
jobEvent :: GitLabBuildEvent -> Spuriobot ()
jobEvent glBuildEvent =
    void $ fork $ withTrace (showt (glbBuildId glBuildEvent)) $ processBuildEvent glBuildEvent

--
-- Helpers
--

showt :: Show a => a -> Text
showt = T.pack . show

-- TODO convert to newtype when the fancy strikes
type Token = ByteString


--
-- Servant boilerplate
--

type WebHookAPI =
    ReqBody '[JSON] GitLabBuildEvent :> Post '[JSON] ()

webhookAPI :: Proxy WebHookAPI
webhookAPI = Proxy
