module Main (main) where

import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import GitLab
import Network.Wai.Handler.Warp (run)
import System.Environment (
    getArgs,
    getEnv,
 )

main :: IO ()
main = do
    args <- getArgs
    envStrApiToken <- getEnv "GITLAB_API_TOKEN"
    case envStrApiToken of
        "" -> error "please set the GITLAB_API_TOKEN environment variable to a valid token string"
        strApiToken ->
            case args of
                [connStr] -> run 8080 $ webhookApplication (textEncode connStr) (textEncode strApiToken)
                _ -> error "Usage: gitlab-webhook pgconnstring"
  where
    textEncode = encodeUtf8 . T.pack
