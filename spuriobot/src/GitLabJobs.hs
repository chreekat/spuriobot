{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module GitLabJobs (fetchJobsBetweenDates) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Maybe
import Data.Text (Text)
import Data.Time
import Database.PostgreSQL.Simple
import Network.HTTP.Req
import qualified Network.HTTP.Req as R
import System.Environment
import Text.URI (mkURI, URI)
import GHC.Generics (Generic)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.List (find, isInfixOf)
import GitLabApi ( Project(..), GitLabToken(..))
import Control.Exception (catch, SomeException)

data JobProject = JobProject { name :: String, projectId :: Int } deriving (Eq, Show)

projects :: [JobProject]
projects =
    [ JobProject "ghc/ghc" 1
    -- Add more JobProjects as needed
    ]

data GlbProject = GlbProject
    { glbProjectId :: Maybe Int
    } deriving (Eq, Show, Generic)

instance FromJSON GlbProject where
    parseJSON = withObject "GlbProject" $ \v -> GlbProject
        <$> v .:? "project_id"  

data Runner = Runner
    { runnerId :: Int
    , runnerName :: Text
    } deriving (Eq, Show, Generic)

instance FromJSON Runner where
    parseJSON = withObject "Runner" $ \v -> Runner
        <$> v .: "id"
        <*> v .: "name"

data Job = Job
    { jobId :: Int
    , jobType :: Maybe Text
    , createdAt :: UTCTime
    , webUrl :: Text
    , runner :: Maybe Runner
    , jobName :: Maybe Text
    , glbProject :: Maybe GlbProject  
    } deriving (Eq, Show, Generic)

instance FromJSON Job where
    parseJSON = withObject "Job" $ \v -> Job
        <$> v .: "id"
        <*> v .:? "stage"
        <*> v .: "created_at"
        <*> v .: "web_url"
        <*> v .:? "runner"
        <*> v .:? "name"
        <*> v .:? "pipeline"

fetchProject :: GitLabToken -> Int -> IO Project
fetchProject (GitLabToken tok) (projectId) = do
    fmap responseBody $ runReq defaultHttpConfig $
        req
            R.GET
            ( https
                "gitlab.haskell.org"
                /: "api"
                /: "v4"
                /: "projects"
                /~ projectId
            )
            NoReqBody
            jsonResponse
            (headerRedacted "PRIVATE-TOKEN" tok)

jobsAPI :: JobProject -> Maybe Int -> URI
jobsAPI (projectId -> i) page = fromJust $ mkURI $
    "https://gitlab.haskell.org/api/v4/projects/"
    <> T.pack (show i)
    <> "/jobs?scope%5B%5D=failed&scope%5b%5d=success&per_page=100"
    <> maybe "" (\p -> "&page=" <> T.pack (show p)) page

api :: URI -> GitLabToken -> Req (JsonResponse [Job])
api uri (GitLabToken key) =
    case fromJust $ useURI uri of
        Left (httpUrl, opts) ->
            req GET httpUrl NoReqBody jsonResponse (opts <> header "PRIVATE-TOKEN" key)
        Right (httpsUrl, opts) ->
            req GET httpsUrl NoReqBody jsonResponse (opts <> header "PRIVATE-TOKEN" key)

fetchJobs :: GitLabToken -> (UTCTime, UTCTime) -> URI -> IO [Job]
fetchJobs token (minDate, maxDate) jobUrl = runReq defaultHttpConfig $ do
    resp <- api jobUrl token
    let jobs = responseBody resp
    pure $ filter (\j -> createdAt j >= minDate && createdAt j <= maxDate) jobs

storeJobs :: Connection -> GitLabToken -> [Job] -> IO ()
storeJobs conn token jobs = do
    let insertQuery = "INSERT INTO ci_job (job_id, type, job_date, web_url, runner_id, runner_name, job_name, project_path) VALUES (?, ?, ?, ?, ?, ?, ?, ?) ON CONFLICT (job_id, type) DO NOTHING"
    
    jobRows <- mapM (fetchAndConvertJob token) jobs
    
    _ <- executeMany conn insertQuery jobRows
    pure ()
  where 
    fetchAndConvertJob token job = do
        let glbProjectId = fromJust $ getGlbProjectId (glbProject job)
        project <- fetchProjectSafe token glbProjectId
        pure $ toRow (getProjPath project) job

    fetchProjectSafe token glbProjectId = catch (liftIO $ fetchProject token glbProjectId) handleFetchError
    
    handleFetchError :: SomeException -> IO Project
    handleFetchError _ = pure (Project "")

    toRow projName Job{..} = (jobId, jobType, createdAt, webUrl, getRunnerId runner, getRunnerName runner, jobName, projName)

    getRunnerId Nothing = Nothing
    getRunnerId (Just r) = Just (runnerId r)
    
    getRunnerName Nothing = Nothing
    getRunnerName (Just r) = Just (runnerName r)
    
    getGlbProjectId Nothing = Nothing
    getGlbProjectId (Just g) = glbProjectId g

    getProjPath :: Project -> Text
    getProjPath (Project path) = path

fetchJobsBetweenDates :: (UTCTime, UTCTime) -> IO ()
fetchJobsBetweenDates dateRange = do
    key <- BS.pack <$> getEnv "GITLAB_API_TOKEN"
    let token = GitLabToken key
    conn <- connectPostgreSQL ""
    mapM_ (fetchAndStoreJobs token conn dateRange) projects
    where
        fetchAndStoreJobs token conn dateRange project = do
            let firstPage = jobsAPI project Nothing
            jobs <- fetchJobs token dateRange firstPage
            storeJobs conn token jobs
            runReq defaultHttpConfig $ fetchRemainingPages token conn dateRange (jobsAPI project (Just 2))

        fetchRemainingPages :: GitLabToken -> Connection -> (UTCTime, UTCTime) -> URI -> Req ()
        fetchRemainingPages token conn dateRange link = do
            resp <- api link token
            let jobs = responseBody resp
            liftIO $ storeJobs conn token jobs
            let nextLink = getNextPageLink resp
            case nextLink of
                Just l -> fetchRemainingPages token conn dateRange (fromJust $ mkURI (T.pack l))
                Nothing -> pure ()

getNextPageLink :: JsonResponse [Job] -> Maybe String
getNextPageLink resp = do
    let headers = responseHeader resp "Link"
        linkHeader = fromMaybe "" headers
        linkHeaderStr = case T.splitOn ", " $ TE.decodeUtf8 linkHeader of
                          [] -> ""
                          (x:_) -> T.unpack x
    find (\link -> "rel=\"next\"" `isInfixOf` link) [linkHeaderStr]