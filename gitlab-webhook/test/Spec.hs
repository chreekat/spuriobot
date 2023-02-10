module Main (main) where

import Control.Concurrent
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Either (
    fromRight,
    isRight,
 )
import qualified Data.Set as S
import Data.Text.IO as T
import Network.HTTP.Client (
    defaultManagerSettings,
    newManager,
 )
import Network.Wai.Handler.Warp (run)
import Servant.Client (
    BaseUrl (..),
    ClientM,
    Scheme (Http),
    client,
    mkClientEnv,
    runClientM,
 )
import Test.Hspec (
    describe,
    hspec,
    it,
    shouldBe,
    shouldSatisfy,
 )

import GitLab

-- for testing the webhook
-- This isn't a great test, because we are only sending a subset of the fields
-- over. However it does test that the webhook server works at all.
postGitLabBuildEvent :: GitLabBuildEvent -> ClientM ()
postGitLabBuildEvent = client webhookAPI

main :: IO ()
main = hspec $ do
    let testWebPort = 9999
    describe "Deserialise GitLab REST data" $ do
        it "can read a GitLabBuildEvent" $ do
            -- TODO use eitherDecodeFilel
            f <- B.readFile "testdata/event.json"
            let eiDecoded = eitherDecode f :: Either String GitLabBuildEvent
            eiDecoded `shouldSatisfy` isRight
    describe "Serve a GitLab webhook" $ do
        -- FIXME this actually hits gitlab; and refernces a job that will be garbage collected eventually
        -- so at least disable this in nix, and probably get rid of it altogether
        it "can accept a GitLabBuildEvent POST" $ do
            event <- fromRight undefined <$> eitherDecodeFileStrict "testdata/event.json"
            t <- forkIO $ run testWebPort (webhookApplication "host=localhost" "fake_api_token")
            ourManager <- newManager defaultManagerSettings
            res <- runClientM (postGitLabBuildEvent event) (mkClientEnv ourManager (BaseUrl Http "localhost" testWebPort ""))
            res `shouldSatisfy` isRight
            killThread t
    describe "Log grepping" $ do
        it "can find a sample error" $ do
            logText <- T.readFile "testdata/kill9failure.log"
            grepForFailures logText `shouldBe` S.fromList [("signal_9", "received signal 9")]
