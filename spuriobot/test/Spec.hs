module Main (Main.main) where

import Data.Aeson
import Data.Either (
    isRight,
 )
import Data.Maybe
import qualified Data.Set as S
import Data.Text.IO as T
import Test.Hspec (
    describe,
    hspec,
    it,
    shouldBe,
    shouldSatisfy,
  )
import Text.URI

import GitLabApi
import Spuriobot.Spurio

main :: IO ()
main = hspec $ do
    describe "Deserialise GitLab REST data" $ do
        it "can read a GitLab Time" $ do
            let time = eitherDecodeStrict' "\"2023-08-21 03:49:15 UTC\""
            time `shouldBe` Right (GitLabTime (read "2023-08-21 03:49:15"))
        it "can read a GitLabBuildEvent" $ do
            eiDecoded <- eitherDecodeFileStrict' "testdata/no-spurio.json" :: IO (Either String GitLabBuildEvent)
            eiDecoded `shouldSatisfy` isRight
        it "can read a job retry response" $ do
            eiDecoded <- eitherDecodeFileStrict' "testdata/retry-response.json" :: IO (Either String RetryResult)
            eiDecoded `shouldSatisfy` isRight
        it "can read a FinishedJob" $ do
            job <- eitherDecodeFileStrict' "testdata/jobinfo.json" :: IO (Either String FinishedJob)
            let url = JobWebURI (fromJust (mkURI "https://gitlab.haskell.org/ghc/ghc/-/jobs/1593505"))
            job `shouldBe` Right
                (FinishedJob
                    url
                    (Just 142)
                    (Just "x86-64-win-2.zw3rk.com")
                    (read "2023-07-07 17:30:53.227")
                    (Just (OtherReason "trace_size_exceeded"))
                    "x86_64-windows-validate")
        it "can read a Project" $ do
            job <- eitherDecodeFileStrict' "testdata/project.json" :: IO (Either String Project)
            job `shouldBe` Right (Project "ghc/ghc")
        describe "System events" $ do
            it "can read a project creation event" $ do
                projCreated <- eitherDecodeFileStrict' "testdata/project-creation-event.json" :: IO (Either String GitLabSystemEvent)
                projCreated `shouldBe` Right (ProjectSystemEvent ProjectCreate (ProjectId 74))
            it "can read a project destroy event" $ do
                projDestroyed <- eitherDecodeFileStrict' "testdata/project-destroyed-event.json" :: IO (Either String GitLabSystemEvent)
                projDestroyed `shouldSatisfy` isRight
            it "can read a user removed event" $ do
                userRemoved <- eitherDecodeFileStrict' "testdata/system-user-removed.json" :: IO (Either String GitLabSystemEvent)
                userRemoved `shouldSatisfy` isRight

    describe "Log grepping" $ do
        it "can find a sample error" $ do
            logText <- T.readFile "testdata/kill9failure.log"
            collectFailures (Jobbo Nothing logText) `shouldBe` S.fromList [("signal_9", "received signal 9")]
