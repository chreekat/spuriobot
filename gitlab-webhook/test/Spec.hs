module Main (main) where

import Data.Aeson
import Data.Either (
    isRight,
 )
import qualified Data.Set as S
import Data.Text.IO as T
import Test.Hspec (
    describe,
    hspec,
    it,
    shouldBe,
    shouldSatisfy,
 )

import GitLab

main :: IO ()
main = hspec $ do
    describe "Deserialise GitLab REST data" $ do
        it "can read a GitLabBuildEvent" $ do
            eiDecoded <- eitherDecodeFileStrict' "testdata/event.json" :: IO (Either String GitLabBuildEvent)
            eiDecoded `shouldSatisfy` isRight
    describe "Log grepping" $ do
        it "can find a sample error" $ do
            logText <- T.readFile "testdata/kill9failure.log"
            grepForFailures logText `shouldBe` S.fromList [("signal_9", "received signal 9")]
