{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}
-- Used for MonadConc:
{-# LANGUAGE UndecidableInstances #-}

module Spuriobot.Spurio (
    collectFailures,
    Check(..),
    Jobbo(..),
    logFailures,
    processBuildEvent,
) where

import Control.Monad.Catch (finally, Exception)
import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (asks)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.Regex.TDFA as Regex
import Data.Int (Int64)
import Control.Monad
import Control.Exception (throwIO)

import qualified Spuriobot.DB as DB


import GitLabApi
import Spuriobot.Foundation
import Spuriobot.RetryJob

--
-- Helpers
--

(=~) :: Text -> Text -> Bool
(=~) = (Regex.=~)


--
-- Controller logic
--

data SpuriobotException = ParseUrlFail
    deriving stock (Eq, Show)
    deriving anyclass Exception

-- the code that we inject into the database
type FailureErrorCode = Text

-- the message we echo to stdout
type FailureMessage = Text

type Failure = (FailureErrorCode, FailureMessage)

data Check = Check
    { checkMsg :: FailureMessage
    , checkCode :: FailureErrorCode
    , checkFn :: Jobbo -> Bool
    }

runCheck :: Jobbo -> Check -> Maybe (FailureMessage, FailureErrorCode)
runCheck j (Check msg cod fn) = if fn j then Just (cod, msg) else Nothing

checkTimeout :: Check
checkTimeout = Check "job timeout" "job_timeout" $ \(Jobbo rs _) ->
    case rs of
        Just JobTimeout -> True
        Just JobStuck -> True
        Just (OtherReason _) -> False
        Nothing -> False

checkLogs :: [Check]
checkLogs =
    let (Jobbo _ logs) !> search = search `T.isInfixOf` logs
        (Jobbo _ logs) ~> search = logs =~ search
    in
        [ Check "docker failure" "docker"
            (!> "Cannot connect to the Docker daemon at unix:///var/run/docker.sock")
        -- TODO: Disabled until the bot gains backoff abilities.
        --, Check "image pull failure" "pull_image"
        --    (!> "failed to pull image \"registry.gitlab.haskell.org")
        , Check "GitLab connection failure" "connect_gitlab"
            (!> "Failed to connect to gitlab.haskell.org")
        , Check "exhausted disk" "no_space"
            (\j -> j !> "No space left on device"
                -- Avoid false positives from T21336 output.
                -- This may cause false negatives, but I think that's the lesser
                -- of two evils.
                && not (j !> "GHC.IO.FD.fdWrite: resource exhausted"
                        || j !> "<stdout>: hFlush: resource exhausted"))
        -- head.hackage#38
        , Check "received signal 9" "signal_9"
            (~> "failed due to signal 9 .Killed")
        -- Modified this search due to ghc#23139. See #9.
        , Check "could not allocate memory" "cannot_allocate"
            (~> "osCommitMemory: VirtualAlloc MEM_COMMIT failed|out of memory allocating \\d+ bytes")
        , Check "MoveFileEx-related failure" "MoveFileEx"
            (!> "MoveFileEx")
        -- #23039
        , Check "Submodule clone failure" "submodule_clone"
            (~> "Failed to clone '.*' a second time, aborting")
        -- #22870
        , Check "ghc-pkg or hadrian failure" "ghc-pkg_died"
            (!> "ghc-pkg dump failed: dieVerbatim: user error")
        -- #22860
        , Check "Nix#7273 failure" "nix_T7273"
            (\t -> t !> "cannot link '/nix/store/.tmp-link"
                || t !> "error: clearing flags of path '/nix/store")
        -- #22408
        , Check "\"cabal exec hadrian\" segfault" "cabal_hadrian_segfault"
            (~> "Segmentation fault.*CABAL.*new-exec.*hadrian")
        -- #22869
        , Check "error code: -6" "code_-6"
            (~> "Command failed with error code: -6")
        , Check "runner terminated" "runner_process_terminated"
            (!> "ERROR: Job failed (system failure): aborted: terminated")
        -- #22967
        , Check "ghc-config file conflict" "ghc-config_file_conflict"
            (~> "posix_spawnp: resource busy")
        -- #22990
        , Check "error code: -11" "code_-11"
            (~> "Command failed with error code: -11")
        -- #21008
        , Check "ulimit: Invalid argument" "ulimit"
            (~> "ulimit: virtual memory: cannot modify limit: Invalid argument")
        -- #23039
        , Check "error cloning fresh repository" "repo_clone"
            (!> "fresh repository.\x1b[0;m\nerror")
        -- #23039 again
        , Check "error fetch perf notes" "perf_note_fetch"
            (!> "refs/notes/perf:refs/notes/perf\nerror:")
        -- #23144
        , Check "death by SIGQUIT" "sigquit"
            (~> "^SIGQUIT: quit")
        ]

-- TODO tests
collectFailures :: Jobbo -> Set Failure
collectFailures j = S.fromList (mapMaybe (runCheck j) (checkTimeout : checkLogs))

logFailures :: Set Failure -> Spuriobot ()
logFailures failures
    | S.null failures = trace "no known spurio"
    | otherwise = forM_
        (S.toList failures)
        ( \(_, msg) -> trace msg)

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

-- | Characteristics of a job that we test against.
data Jobbo = Jobbo (Maybe JobFailureReason) Text

-- | Given a failed job, deal with spurios (if any)
processFailure :: GitLabBuildEvent -> Spuriobot ()
processFailure GitLabBuildEvent { glbProjectId, glbBuildId } = do
    tok <- asks apiToken
    jobInfo <- liftIO $ fetchJobInfo tok glbProjectId glbBuildId
    logs <- do
        l <- liftIO $ fetchJobLogs tok (webUrl jobInfo)
        case l of
            -- this error will only terminate the forked processFailure thread,
            -- so the main process should keep listening and handling requests.
            -- FIXME: use throwError in the Handler monad
            Left (JobWebUrlParseFailure url) -> do
                trace $ "error: could not parse URL: " <> url
                liftIO $ throwIO ParseUrlFail
            Right logs -> pure logs
    let jobbo = Jobbo (jobFailureReason jobInfo) logs
    let failures = collectFailures jobbo
    logFailures failures
    void $ runDB $ DB.insertFailures (mkDBFailures glbBuildId jobInfo (S.toList failures))
    unless (S.null failures) $
        withTrace "retrying" $ retryJob glbProjectId glbBuildId

-- | Map between our types and the DB's types
mkDBFailures :: Functor f => Int64 -> JobInfo -> f Failure -> f DB.Failure
mkDBFailures jobId JobInfo { jobDate, webUrl, runnerId } fails =
    let mk (code, _) = DB.Failure jobId code jobDate webUrl runnerId
    in fmap mk fails
