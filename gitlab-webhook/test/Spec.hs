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
import Servant.Client
import Test.Hspec

import GL

main :: IO ()
main = hspec $ do
    let testWebPort = 9999
    describe "Deserialise GL REST data" $ do
        it "can read a GLBuildEvent" $ do
            -- TODO use eitherDecodeFilel
            f <- B.readFile "testdata/event.json"
            let eiDecoded = eitherDecode f :: Either String GLBuildEvent
            eiDecoded `shouldSatisfy` isRight
    describe "Serve a GL webhook" $ do
        it "can accept a GLBuildEvent POST" $ do
            event <- fromRight undefined <$> eitherDecodeFileStrict "testdata/event.json"
            t <- forkIO $ run testWebPort (webhookApplication "host=localhost" "fake_api_token")
            ourManager <- newManager defaultManagerSettings
            res <- runClientM (postGLBuildEvent event) (mkClientEnv ourManager (BaseUrl Http "localhost" testWebPort ""))
            res `shouldSatisfy` isRight
            killThread t
    describe "Log grepping" $ do
        it "can find a sample error" $ do
            logText <- T.readFile "testdata/kill9failure.log"
            grepForFailures logText `shouldBe` S.fromList [("signal_9", "received signal 9")]

-- TODO: more tests..
