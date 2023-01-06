{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module GL where

-- FIXME constrain imports

import Control.Exception (throwIO)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Proxy
import Network.HTTP.Req as R
import Network.Wai
import Servant
import Servant.Client (ClientM, client)

type WebHookAPI = ReqBody '[JSON] GLBuildEvent :> Post '[JSON] ()

webhookServer :: Server WebHookAPI
webhookServer glBuildEvent = do
    liftIO $ print glBuildEvent

webhookAPI :: Proxy WebHookAPI
webhookAPI = Proxy

webhookApplication :: Application
webhookApplication = serve webhookAPI webhookServer

-- for testing the webhook
-- This isn't a great test, because we are only sending a subset of the fields
-- over. However it does test that the webhook server works at all.
postGLBuildEvent :: GLBuildEvent -> ClientM ()
postGLBuildEvent = client webhookAPI

data GLCommit = GLCommit
    { glcId :: Int
    , glcSha :: String
    , glcMessage :: String
    , glcAuthorName :: String
    }
    deriving (Show, Ord, Eq)

instance FromJSON GLCommit where
    parseJSON = withObject "GLCommit" $ \v ->
        GLCommit
            <$> v
            .: "id"
            <*> v
            .: "sha"
            <*> v
            .: "message"
            <*> v
            .: "author_name"

instance ToJSON GLCommit where
    toJSON x =
        object
            [ "id" .= glcId x
            , "sha" .= glcSha x
            , "message" .= glcMessage x
            , "author_name" .= glcAuthorName x
            ]

data GLBuildEvent = GLBuildEvent
    { glbRef :: String
    , glbBuildId :: Int
    , glbBuildName :: String
    , glbBuildStatus :: String -- FIXME
    , glbBuildFailureReason :: String
    , glbProjectId :: Int
    , glbCommit :: GLCommit
    }
    deriving (Show, Ord, Eq)

-- FIXME fourmolu too opinionated here
instance FromJSON GLBuildEvent where
    parseJSON = withObject "GLBuildEvent" $ \v ->
        GLBuildEvent
            <$> v
            .: "ref"
            <*> v
            .: "build_id"
            <*> v
            .: "build_name"
            <*> v
            .: "build_status"
            <*> v
            .: "build_failure_reason"
            <*> v
            .: "project_id"
            <*> v
            .: "commit"

instance ToJSON GLBuildEvent where
    toJSON x =
        object
            [ "ref" .= glbRef x
            , "build_id" .= glbBuildId x
            , "build_name" .= glbBuildName x
            , "build_status" .= glbBuildStatus x
            , "build_failure_reason" .= glbBuildFailureReason x
            , "project_id" .= glbProjectId x
            , "commit" .= glbCommit x
            ]

type BuildId = Int

-- need to write a type for the expected response from the query

fetchJobLogs :: BuildId -> IO String
fetchJobLogs buildId = runReq defaultHttpConfig $ do
    r <- req R.GET (https "gitlab.haskell.org" /: "shtuf") NoReqBody jsonResponse mempty
    pure $ show (r :: JsonResponse ()) -- FIXME placeholder type
