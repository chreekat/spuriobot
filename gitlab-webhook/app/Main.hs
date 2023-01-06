module Main (main) where

import Network.Wai.Handler.Warp

import GL

main :: IO ()
main = run 8080 webhookApplication
