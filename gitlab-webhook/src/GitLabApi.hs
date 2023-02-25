module GitLabApi (
    JobId,
    JobInfo (..),
    JobWebURL,
    ProjectId (..),
    Token,
    fetchJobInfo,
) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (
    FromJSON,
    parseJSON,
    withObject,
    (.:),
 )
import Data.ByteString (ByteString)
import Data.Text (Text)
import Network.HTTP.Req (
    NoReqBody (..),
    defaultHttpConfig,
    headerRedacted,
    https,
    jsonResponse,
    req,
    responseBody,
    runReq,
    (/:),
 )
import qualified Network.HTTP.Req as R
import TextShow (showt)

newtype ProjectId = ProjectId {unProjectId :: Int} deriving (Show, Ord, Eq)
type JobId = Int
type JobWebURL = Text
type Token = ByteString

-- Sparse definition
newtype JobInfo = JobInfo
    { webUrl :: JobWebURL
    }
    deriving (Show, Ord, Eq)

instance FromJSON JobInfo where
    parseJSON = withObject "JobInfo" $ \o ->
        JobInfo <$> o .: "web_url"

fetchJobInfo :: Token -> ProjectId -> JobId -> IO JobInfo
fetchJobInfo apiToken (ProjectId projectId) jobId = runReq defaultHttpConfig $ do
    response <-
        req
            R.GET
            ( https
                "gitlab.haskell.org"
                /: "api"
                /: "v4"
                /: "projects"
                /: showt projectId
                /: "jobs"
                /: showt jobId
            )
            NoReqBody
            jsonResponse
            (headerRedacted "PRIVATE-TOKEN" apiToken)
    liftIO $ pure (responseBody response)
