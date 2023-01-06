module Main where

import Control.Concurrent
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Either (
    fromRight,
    isRight,
 )
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
    describe "Deserialise GL REST data" $ do
        it "can read a GLBuildEvent" $ do
            -- TODO use eitherDecodeFilel
            f <- B.readFile "testdata/event.json"
            let eiDecoded = eitherDecode f :: Either String GLBuildEvent
            eiDecoded `shouldSatisfy` isRight
    describe "Serve a GL webhook" $ do
        it "can accept a GLBuildEvent POST" $ do
            event <- fromRight undefined <$> eitherDecodeFileStrict "testdata/event.json"
            t <- forkIO $ run 8080 webhookApplication
            manager <- newManager defaultManagerSettings
            res <- runClientM (postGLBuildEvent event) (mkClientEnv manager (BaseUrl Http "localhost" 8080 ""))
            res `shouldSatisfy` isRight
            killThread t
