{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module Spuriobot.SearchUI (searchUIServer, searchAPI) where

import Web.Scotty
import Control.Monad.IO.Class (liftIO)
import Database.SQLite.Simple
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Format (formatTime, defaultTimeLocale)
import Lucid
import Control.Concurrent.STM (TMVar, readTMVar, atomically)
import Servant.API
import Servant.Server (Server)
import Data.Proxy (Proxy(..))
import Control.Monad (when)
import Data.Int (Int64)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

-- Define JobInfo data type
data JobInfo = JobInfo
  { jobId :: Int64
  , createdAt :: UTCTime
  , webUrl :: Text
  , runnerId :: Maybe Int64
  , runnerName :: Maybe Text
  , jobName :: Text
  , projectPath :: Text
  } deriving (Generic, Show)

-- Define the Search API
type SearchAPI = "search" :> QueryParam "keyword" Text :> Get '[PlainText] (Html ())

-- Create a Proxy for the Search API
searchAPI :: Proxy SearchAPI
searchAPI = Proxy

-- HTML rendering function using Lucid
renderJob :: JobInfo -> Html ()
renderJob job = 
  div_ $ do
    h2_ $ toHtml (jobName job)
    p_ $ do
      "Job Id: " >> toHtml (T.pack $ show (jobId job))
      br_ []
      "Date: " >> toHtml (T.pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" (createdAt job))
      br_ []
      "URL: " >> toHtml (webUrl job)
      br_ []
      "Runner Id: " >> toHtml (maybe "N/A" (T.pack . show) (runnerId job))
      br_ []
      "Runner Name: " >> toHtml (maybe "N/A" id (runnerName job))
      br_ []
      "Project Path: " >> toHtml (projectPath job)

-- HTML template for the search form and results
renderPage :: Text -> [JobInfo] -> Bool -> Int -> Html ()
renderPage keyword results hasNextPage nextPage = do
  doctype_
  html_ $ do
    head_ $ do
      title_ "Job Search"
      script_ $ "function nextPage(page) { window.location.href = '/search?page=' + page + '&keyword=' + '" <> keyword <> "'; }"
    body_ $ do
      h1_ "Job Search"
      form_ [method_ "get"] $ do
        input_ [type_ "hidden", name_ "page", value_ "1"]
        input_ [type_ "text", name_ "keyword", value_ keyword]
        button_ [type_ "submit"] "Search"
      hr_ []
      if null results
        then p_ "No results found."
        else do
          mapM_ renderJob results
          when (nextPage > 2) $ -- Previous Page button should not be on the first page
            button_ [onclick_ $ "nextPage(" <> T.pack (show (nextPage - 2)) <> ")"] "Previous Page"
          when hasNextPage $
            button_ [onclick_ $ "nextPage(" <> T.pack (show nextPage) <> ")"] "Next Page"

-- Function to wrap the keyword in quotes
wrapKeyword :: Maybe Text -> Maybe Text
wrapKeyword = fmap (\k -> "\"" <> k <> "\"")

-- Database query to fetch job results based on the keyword
searchJobs :: Connection -> Maybe Text -> Int -> Int -> IO ([JobInfo], Int)
searchJobs conn (Just keyword) limit offset = do
  let wrappedKeyword = wrapKeyword (Just keyword)
      countQry = "SELECT COUNT(*) FROM job WHERE job_id IN (SELECT rowid FROM job_trace WHERE trace MATCH ?);"
      dataQry = "SELECT job_id, job_date, web_url, runner_id, runner_name, job_name, project_path \
                \FROM job WHERE job_id IN (SELECT rowid FROM job_trace WHERE trace MATCH ?) \
                \ORDER BY job_date DESC LIMIT ? OFFSET ?;"
  totalRows <- query conn countQry (Only wrappedKeyword)
  rows <- query conn dataQry (wrappedKeyword, limit, offset)
  return (map (\(jid, jdate, url, rid, rname, jname, path) -> JobInfo jid jdate url rid rname jname path) rows, fromOnly (head totalRows))

searchJobs conn Nothing limit offset = do
  let countQry = "SELECT COUNT(*) FROM job;"
      dataQry = "SELECT job_id, job_date, web_url, runner_id, runner_name, job_name, project_path \
                \FROM job ORDER BY job_date DESC LIMIT ? OFFSET ?;"
  totalRows <- query conn countQry (limit, offset)
  rows <- query conn dataQry (limit, offset)
  return (map (\(jid, jdate, url, rid, rname, jname, path) -> JobInfo jid jdate url rid rname jname path) rows, fromOnly (head totalRows))

-- Scotty server for the search UI
searchUIServer :: TMVar Connection -> ScottyM ()
searchUIServer connVar = do
  get "/search" $ do
    -- Get the keyword parameter, default to an empty string if not provided
    mKeyword <- param "keyword" `rescue` (\(_ :: ScottyException) -> return "")
    page <- param "page" `rescue` (\(_ :: ScottyException) -> return 1)
    
    -- Ensure the keyword is always a Text value, even if empty
    let keyword = if T.null mKeyword then "" else mKeyword
        pageSize = 50
        offset = (page - 1) * pageSize
        
    conn <- liftIO $ atomically $ readTMVar connVar
    (jobs, totalCount) <- liftIO $ searchJobs conn (Just keyword) pageSize offset
    let hasNextPage = totalCount > offset + pageSize
    html $ renderText $ renderPage keyword jobs hasNextPage (page + 1)

