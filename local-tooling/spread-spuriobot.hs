{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main (main) where

import Control.Applicative
import Data.Aeson
import Data.Function
import Data.Maybe
import Data.Text (Text)
import Data.Word
import List.Transformer as List
import Network.HTTP.Req
import System.Environment
import System.IO
import Text.URI
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

headMay (a:_) = Just a
headMay [] = Nothing

data Project = Project 
    { pid :: Word64
    , path :: Text
    } deriving (Eq, Show)

instance FromJSON Project where
    parseJSON = withObject "Project" $ \o ->
        Project
            <$> o .: "id"
            <*> o .: "path_with_namespace"

data Hook = Hook
    { hid :: Word64
    , url :: Text
    } deriving (Eq, Show)

instance FromJSON Hook where
    parseJSON = withObject "Hook" $ \o ->
        Hook
            <$> o .: "id"
            <*> o .: "url"

instance ToJSON Hook where
    toJSON Hook { url } = object
        [ "url" .= url
        , "job_events" .= True
        , "push_events" .= False
        , "enable_ssl_verification" .= False
        ]
    toEncoding Hook { url } = pairs
        ( "url" .= url
        <> "job_events" .= True
        <> "push_events" .= False
        <> "enable_ssl_verification" .= False
        )

data Paged a
    = WithMore URI [a]
    | NoMore [a]
    deriving (Eq, Show)

projectsUrlRoot = https "gitlab.haskell.org" /: "api" /: "v4" /: "projects"

glapi typ bod resp url opts = do
    strApiToken <- T.encodeUtf8 . T.pack <$> getEnv "GITLAB_API_TOKEN"
    runReq defaultHttpConfig $ req typ url bod resp (opts <> header "PRIVATE-TOKEN" strApiToken)

glGetApi :: FromJSON a => Url scheme -> Option scheme -> IO (JsonResponse a)
glGetApi = glapi GET NoReqBody jsonResponse

glPostApi url opts bod = glapi POST (ReqBodyJson bod) ignoreResponse url opts

hasSpuriobot :: Foldable f => f Hook -> Bool
hasSpuriobot = any ((":8080" `T.isInfixOf`) . url)

projectsUrl :: Maybe URI -> (Url 'Https, Option w)
projectsUrl murl =
    let url = projectsUrlRoot
        defaultOptions =
            "pagination" =: ("keyset" :: String)
            <> "per_page" =: (100 :: Word)
            <> "order_by" =: ("id" :: String)
            <> "sort" =: ("desc" :: String)
        res = useHttpsURI =<< murl
    in fromMaybe (url, defaultOptions) res

projectHooks :: Project -> IO [Hook]
projectHooks Project { pid } = do
    responseBody <$> glGetApi url mempty
    where
        url = projectsUrlRoot /~ pid /: "hooks"

fetchProjectPage :: Maybe URI -> IO (Paged Project)
fetchProjectPage mpage = do
    strApiToken <- T.encodeUtf8 . T.pack <$> getEnv "GITLAB_API_TOKEN"
    let (url, opts) = projectsUrl mpage
    response <- glGetApi url opts
    nextLink <- headMay <$> responseLinks "rel" "next" response
    let projs = responseBody response
    pure $ maybe NoMore WithMore nextLink projs

fetchAllProjects :: ListT IO Project
fetchAllProjects = fix f Nothing
    where
        f nxt page = do
            pp <- lift $ fetchProjectPage page
            case pp of
                WithMore uri ps -> select ps <|> nxt (Just uri)
                NoMore ps -> select ps

addSpuriobot :: Project -> IO ()
addSpuriobot Project { pid } = do
    responseBody <$> glPostApi (projectsUrlRoot /~ pid /: "hooks") mempty (Hook 1 "http://127.0.0.1:8080/spuriobot")
    putStrLn ": added spuriobot"

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    runListT $ do
        p <- fetchAllProjects
        lift $ putStr (show (pid p))
        h <- lift $ projectHooks p
        if hasSpuriobot h
            then lift $ putStrLn ": SPURIOBOT PRESENT"
            else lift $ addSpuriobot p
