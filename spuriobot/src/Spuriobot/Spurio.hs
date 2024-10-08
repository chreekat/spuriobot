{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
-- Used for MonadConc:
{-# LANGUAGE UndecidableInstances #-}

-- | This module is for handling spurious failures and retrying failed jobs.
module Spuriobot.Spurio (
    collectFailures,
    Check(..),
    Jobbo(..),
    logFailures,
    processFailure,
) where

import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.Regex.TDFA as Regex
import Control.Monad
import Text.URI (render)

import GitLabApi
import Spuriobot.Foundation
import Spuriobot.RetryJob
import qualified Spuriobot.DB as DB

--
-- Helpers
--

(=~) :: Text -> Text -> Bool
(=~) = (Regex.=~)


--
-- Controller logic
--

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

checkTimeout, checkRunnerSystem :: Check

-- | checkTimeout used to check for job timeout, but why? Most timeouts are not
-- spurious. Now we're stuck with the name, even though it only lives on to
-- check for stuck jobs. Hm. But I can migrate the name. TODO.
checkTimeout = Check "job timeout" "job_timeout" $ \(Jobbo rs _) ->
    case rs of
        Just JobStuck -> True
        _ -> False

checkRunnerSystem = Check "runner failure" "runner_system" $ \(Jobbo rs _) ->
    case rs of
        Just RunnerSystemFailure -> True
        _ -> False

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
        -- TODO: Disabled because of false positives. See GHC#22022.
        -- , Check "exhausted disk" "no_space"
        --     (!> "No space left on device")
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
        -- #24274
        , Check "stack #6388" "stack_T6388"
            (!> "Error: [S-922]")
        -- #24365
        , Check "Hadrian exception: locked file" "hadrian_locked_file"
            (\t -> t !> "withFile: resource busy (file is locked)"
                || t !> "copyFileToHandle:openFile: resource busy (file is locked)")
        -- #24420
        , Check "error code: -4" "code_-4"
            (!> "Command failed with error code: -4")
        -- #24417
        , Check "wasm32-wasi-ghc-pkg recache" "wasm_ghc-pkg"
            (!> "wasm32-wasi-ghc-pkg: createProcess: exec: invalid argument (Bad file descriptor)")
        ]

-- TODO tests
collectFailures :: Jobbo -> Set Failure
collectFailures j = S.fromList (mapMaybe (runCheck j) (checkTimeout : checkRunnerSystem : checkLogs))

logFailures :: Set Failure -> Spuriobot ()
logFailures failures
    | S.null failures = Spuriobot.Foundation.trace "no known spurio"
    | otherwise = forM_
        (S.toList failures)
        ( \(_, msg) -> Spuriobot.Foundation.trace msg)

-- | Characteristics of a job that we test against.
-- TODO: Get rid of this type, just use the FinishedJob god-object
data Jobbo = Jobbo (Maybe JobFailureReason) Text

-- | Given a failed job, deal with spurios (if any)
processFailure
    :: FinishedJob
    -> Spuriobot ()
processFailure finishedJob@(FinishedJob { finishedJobId, finishedJobProjectId }) = do
    let jobbo = Jobbo (finishedJobFailureReason finishedJob) (finishedJobLogs finishedJob)
    let failures = collectFailures jobbo
    logFailures failures
    void $ runDB $ DB.insertFailures (mkDBFailures finishedJob (S.toList failures))
    unless (S.null failures) $
        withTrace "retrying" $ retryJob finishedJobProjectId finishedJobId

-- | Map between our types and the DB's types
mkDBFailures :: Functor f => FinishedJob -> f Failure -> f DB.Failure
mkDBFailures FinishedJob { finishedJobId, finishedJobFinishedAt, finishedJobWebUrl, finishedJobRunnerId, finishedJobRunnerName, finishedJobName, finishedJobProjectPath } fails =
    let mk (code, _) = DB.Failure finishedJobId code finishedJobFinishedAt (render' finishedJobWebUrl) finishedJobRunnerId finishedJobRunnerName finishedJobName finishedJobProjectPath
        render' (JobWebURI uri) = render uri
    in fmap mk fails
