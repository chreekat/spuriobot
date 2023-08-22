{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Data.Aeson
import Data.Text (Text)
import Data.Word
import Network.HTTP.Req
import System.Environment
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

data Project = Project 
    { id :: Word64
    , path :: Text
    } deriving (Eq, Show)

instance FromJSON Project where
    parseJSON = withObject "Project" $ \o ->
        Project
            <$> o .: "id"
            <*> o .: "path_with_namespace"

main :: IO ()
main = do
    strApiToken <- T.encodeUtf8 . T.pack <$> getEnv "GITLAB_API_TOKEN"
    let url = https "gitlab.haskell.org" /: "api" /: "v4" /: "projects"
    ps :: [Project] <- fmap responseBody $ runReq defaultHttpConfig $ req
        GET
        url
        NoReqBody
        jsonResponse
        ("simple" =: True <> "per_page" =: (100 :: Word) <> header "PRIVATE_TOKEN" strApiToken)
    print ps
