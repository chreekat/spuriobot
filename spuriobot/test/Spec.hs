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
        describe "Read Job" $ do
            it "can read a Job" $ do
                job <- eitherDecodeFileStrict' "testdata/jobinfo.json" :: IO (Either String Job)
                let url = JobWebURI (fromJust (mkURI "https://gitlab.haskell.org/ghc/ghc/-/jobs/1593505"))
                job `shouldBe` Right
                    (Job
                        1593505
                        url
                        (Just 142)
                        (Just "x86-64-win-2.zw3rk.com")
                        (read "2023-07-07 11:37:37.576")
                        (Just (read "2023-07-07 17:30:53.227"))
                        (Just (OtherReason "trace_size_exceeded"))
                        "x86_64-windows-validate"
                        "{\"allow_failure\":false,\"artifacts\":[{\"file_format\":null,\"file_type\":\"trace\",\"filename\":\"job.log\",\"size\":104211623}],\"artifacts_expire_at\":\"2023-07-21T17:30:46.783Z\",\"commit\":{\"author_email\":\"sam.derbyshire@gmail.com\",\"author_name\":\"sheaf\",\"authored_date\":\"2023-07-07T13:37:27.000+02:00\",\"committed_date\":\"2023-07-07T13:37:27.000+02:00\",\"committer_email\":\"sam.derbyshire@gmail.com\",\"committer_name\":\"sheaf\",\"created_at\":\"2023-07-07T13:37:27.000+02:00\",\"id\":\"04b1b538623b38a3969dabfe563129484ef58b98\",\"message\":\"Refactor lookupGRE_... functions\\n\\nThis commit consolidates all the logic for looking up something in\\nthe Global Reader Environment into the single function lookupGRE.\\nThis allows us to declaratively specify all the different modes of\\nlooking up in the GlobalRdrEnv, and avoids manually passing around\\nfiltering functions as was the case in e.g. the function\\nGHC.Rename.Env.lookupSubBndrOcc_helper.\\n\",\"parent_ids\":[\"ec3824446fb2190666d80ed502fe2cc45a5eabab\"],\"short_id\":\"04b1b538\",\"title\":\"Refactor lookupGRE_... functions\",\"trailers\":{},\"web_url\":\"https://gitlab.haskell.org/ghc/ghc/-/commit/04b1b538623b38a3969dabfe563129484ef58b98\"},\"coverage\":null,\"created_at\":\"2023-07-07T11:37:37.576Z\",\"duration\":7280.792147,\"erased_at\":null,\"failure_reason\":\"trace_size_exceeded\",\"finished_at\":\"2023-07-07T17:30:53.227Z\",\"id\":1593505,\"name\":\"x86_64-windows-validate\",\"pipeline\":{\"created_at\":\"2023-07-07T11:37:37.438Z\",\"id\":80914,\"iid\":53663,\"project_id\":1,\"ref\":\"refs/merge-requests/10761/head\",\"sha\":\"04b1b538623b38a3969dabfe563129484ef58b98\",\"source\":\"merge_request_event\",\"status\":\"failed\",\"updated_at\":\"2023-07-10T06:14:57.600Z\",\"web_url\":\"https://gitlab.haskell.org/ghc/ghc/-/pipelines/80914\"},\"project\":{\"ci_job_token_scope_enabled\":false},\"queued_duration\":1.839898,\"ref\":\"refs/merge-requests/10761/head\",\"runner\":{\"active\":true,\"description\":\"x86-64-win-2.zw3rk.com\",\"id\":142,\"ip_address\":\"135.181.117.40\",\"is_shared\":true,\"name\":\"gitlab-runner\",\"online\":true,\"paused\":false,\"runner_type\":\"instance_type\",\"status\":\"online\"},\"stage\":\"full-build\",\"started_at\":\"2023-07-07T15:29:32.435Z\",\"status\":\"failed\",\"tag\":false,\"tag_list\":[\"new-x86_64-windows\"],\"user\":{\"avatar_url\":\"https://secure.gravatar.com/avatar/0cfe3de23679d68748d82750de1b88dc?s=80&d=identicon\",\"bio\":\"\",\"bot\":false,\"created_at\":\"2019-03-10T04:05:54.786Z\",\"discord\":\"\",\"followers\":2,\"following\":0,\"id\":3199,\"job_title\":\"\",\"linkedin\":\"\",\"local_time\":null,\"location\":\"\",\"name\":\"sheaf\",\"organization\":\"\",\"pronouns\":\"he/him\",\"public_email\":\"sam.derbyshire@gmail.com\",\"skype\":\"\",\"state\":\"active\",\"twitter\":\"samderbyshire\",\"username\":\"sheaf\",\"web_url\":\"https://gitlab.haskell.org/sheaf\",\"website_url\":\"\",\"work_information\":null},\"web_url\":\"https://gitlab.haskell.org/ghc/ghc/-/jobs/1593505\"}")
            it "can read a job that failed with RunnerSystemFailure" $ do
                job <- eitherDecodeFileStrict' "testdata/spurio-system_failure.json" :: IO (Either String Job)
                jobFailureReason <$> job `shouldBe` Right (Just RunnerSystemFailure)
        it "can read a Project" $ do
            job <- eitherDecodeFileStrict' "testdata/project.json" :: IO (Either String Project)
            job `shouldBe` Right (Project "ghc/ghc" (ProjectId 1))
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
