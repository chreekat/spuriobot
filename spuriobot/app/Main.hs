module Main (main) where

import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Network.Wai.Handler.Warp (run)
import System.IO (hSetBuffering, stdout, BufferMode(..))
import System.Environment (
    getArgs,
    getEnv,
 )
import Spuriobot
import qualified Spuriobot.DB as DB

main :: IO ()
main = do
    -- Ensure journald gets our output
    hSetBuffering stdout NoBuffering

    -- Fail early if the database connection doesn't work.
    DB.close =<< DB.connect

    args <- getArgs
    envStrApiToken <- getEnv "GITLAB_API_TOKEN"
    case envStrApiToken of
        "" -> error "please set the GITLAB_API_TOKEN environment variable to a valid token string"
        strApiToken ->
            case args of
                [] -> run 8080 $ webhookApplication (textEncode strApiToken)
                _ -> error "Usage: spuriobot"
  where
    textEncode = encodeUtf8 . T.pack
