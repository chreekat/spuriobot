module Main (main) where

import GL
import Network.Wai.Handler.Warp (run)
import System.Environment (
    getArgs,
    getEnv,
 )

main :: IO ()
main = do
    args <- getArgs
    envStrApiToken <- getEnv "GL_API_TOKEN"
    case envStrApiToken of
        "" -> error "please set the GL_API_TOKEN environment variable to a valid token string"
        strApiToken ->
            case args of
                [connStr] -> run 8080 (webhookApplication connStr strApiToken)
                _ -> error "Usage: gitlab-webhook pgconnstring"
