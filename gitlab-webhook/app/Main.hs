module Main (main) where

import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Network.Wai.Handler.Warp (run)
import System.Environment (
    getArgs,
    getEnv,
 )

import SpuriousFailures

main :: IO ()
main = do
    args <- getArgs
    envStrApiToken <- getEnv "GITLAB_API_TOKEN"
    case envStrApiToken of
        "" -> error "please set the GITLAB_API_TOKEN environment variable to a valid token string"
        strApiToken ->
            case args of
                [] -> run 8080 $ webhookApplication (textEncode strApiToken)
                _ -> error "Usage: gitlab-webhook"
  where
    textEncode = encodeUtf8 . T.pack
