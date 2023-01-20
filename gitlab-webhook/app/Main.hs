module Main (main) where

import Network.Wai.Handler.Warp

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GL
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [connStr] -> run 8080 (webhookApplication (T.encodeUtf8 . T.pack $ connStr))
        _ -> error "Usage: gitlab-webhook pgconnstring"
