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

-- Define the SearchResults data type
data SearchResults a = NoSearch | SearchResults [a]
  deriving (Show, Generic)

-- Define the Search API
type SearchAPI = "search" :> QueryParam "keyword" Text :> Get '[PlainText] (Html ())

-- Create a Proxy for the Search API
searchAPI :: Proxy SearchAPI
searchAPI = Proxy

-- HTML rendering function using Lucid
renderJob :: JobInfo -> Html ()
renderJob job = 
  div_ [class_ "p-4 bg-white rounded-lg shadow-md mb-4"] $ do
    h2_ [class_ "text-xl font-semibold text-gray-800"] $ toHtml (jobName job)
    p_ [class_ "text-gray-600"] $ do
      "Job Id: " >> toHtml (show (jobId job))
      br_ []
      "Date: " >> toHtml (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" (createdAt job))
      br_ []
      "URL: " >> a_ [href_ (webUrl job), class_ "text-blue-500 hover:underline"] (toHtml (webUrl job))
      br_ []
      "Runner Id: " >> toHtml (maybe "N/A" show (runnerId job))
      br_ []
      "Runner Name: " >> toHtml (maybe "N/A" id (runnerName job))
      br_ []
      "Project Path: " >> toHtml (projectPath job)

-- HTML template for the search form and results
renderPage :: Text -> SearchResults JobInfo -> Bool -> Int -> Html ()
renderPage keyword results' hasNextPage nextPage = do
  doctype_
  html_ [lang_ "en"] $ do
    head_ $ do
      title_ "Job Search"
      link_ [rel_ "stylesheet", href_ "https://cdn.jsdelivr.net/npm/tailwindcss@2.2.19/dist/tailwind.min.css"]
    body_ [class_ "bg-gray-100 text-gray-900"] $ do
      div_ [class_ "max-w-7xl mx-auto p-4"] $ do
        h1_ [class_ "text-3xl font-bold mb-4"] "Job Search"
        form_ [method_ "get", class_ "mb-6"] $ do
          input_ [type_ "hidden", name_ "page", value_ "1"]
          input_ [type_ "text", name_ "keyword", value_ keyword, class_ "p-2 border border-gray-300 rounded-lg w-full"]
          button_ [type_ "submit", class_ "mt-2 p-2 bg-blue-500 text-white rounded-lg hover:bg-blue-600"] "Search"
        hr_ [class_ "my-4"]

        case results' of
          NoSearch -> pure ()  -- Don't show anything if no search has been made
          SearchResults [] -> p_ [class_ "text-gray-700"] "No results found."
          SearchResults results -> do
            div_ [class_ "space-y-4"] $
              mapM_ renderJob results
            div_ [class_ "flex justify-between mt-6"] $ do
              when (nextPage > 2) $
                form_ [method_ "get", class_ "inline"] $ do
                  input_ [type_ "hidden", name_ "page", value_ (T.pack (show (nextPage - 2)))]
                  input_ [type_ "hidden", name_ "keyword", value_ keyword]
                  button_ [type_ "submit", class_ "p-2 bg-gray-300 text-gray-700 rounded-lg hover:bg-gray-400"] "Previous Page"
              when hasNextPage $
                form_ [method_ "get", class_ "inline"] $ do
                  input_ [type_ "hidden", name_ "page", value_ (T.pack (show nextPage))]
                  input_ [type_ "hidden", name_ "keyword", value_ keyword]
                  button_ [type_ "submit", class_ "p-2 bg-blue-500 text-white rounded-lg hover:bg-blue-600"] "Next Page"

-- Function to wrap the keyword in quotes
wrapKeyword :: Maybe Text -> Maybe Text
wrapKeyword = fmap (\k -> "\"" <> k <> "\"")

-- Database query to fetch job results based on the keyword
searchJobs :: Connection -> Maybe Text -> Int -> Int -> IO (SearchResults JobInfo, Int)
searchJobs conn (Just keyword) limit offset = do
  let wrappedKeyword = wrapKeyword (Just keyword)
      countQry = "SELECT COUNT(*) FROM job WHERE job_id IN (SELECT rowid FROM job_trace WHERE trace MATCH ?);"
      dataQry = "SELECT job_id, job_date, web_url, runner_id, runner_name, job_name, project_path \
                \FROM job WHERE job_id IN (SELECT rowid FROM job_trace WHERE trace MATCH ?) \
                \ORDER BY job_date DESC LIMIT ? OFFSET ?;"
  totalRows <- query conn countQry (Only wrappedKeyword)
  rows <- query conn dataQry (wrappedKeyword, limit, offset)
  let jobs = map (\(jid, jdate, url, rid, rname, jname, path) -> JobInfo jid jdate url rid rname jname path) rows
  return (SearchResults jobs, fromOnly (head totalRows))

searchJobs conn Nothing limit offset = do
  let countQry = "SELECT COUNT(*) FROM job;"
      dataQry = "SELECT job_id, job_date, web_url, runner_id, runner_name, job_name, project_path \
                \FROM job ORDER BY job_date DESC LIMIT ? OFFSET ?;"
  totalRows <- query conn countQry ()
  rows <- query conn dataQry (limit, offset)
  let jobs = map (\(jid, jdate, url, rid, rname, jname, path) -> JobInfo jid jdate url rid rname jname path) rows
  return (SearchResults jobs, fromOnly (head totalRows))

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

    -- If keyword is empty, no search has been performed
    results' <- if T.null keyword
      then return NoSearch
      else do
        (searchResults, totalCount) <- liftIO $ searchJobs conn (Just keyword) pageSize offset
        return searchResults

    let hasNextPage = case results' of
                        SearchResults jobs -> length jobs == pageSize
                        _ -> False

    html $ renderText $ renderPage keyword results' hasNextPage (page + 1)
