{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module Spuriobot.SearchUI (searchUIServer, searchAPI) where

import Web.Scotty
import Database.SQLite.Simple (Connection, Only(..), query)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Format (formatTime, defaultTimeLocale)
import Lucid
import Control.Concurrent.STM (TMVar, readTMVar, atomically)
import Servant.API
import Data.Proxy (Proxy(..))
import Control.Monad (when)
import Data.Int (Int64)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Control.Exception (try, SomeException)
import Data.Maybe (fromMaybe)

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

-- Define a type to handle possible outcomes during search
data SearchOutcome a = SearchError Text | SearchSuccess (SearchResults a)
  deriving (Show, Generic)

-- Define the Search API
type SearchAPI = "search" :> QueryParam "q" Text :> Get '[PlainText] (Html ())

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
renderPage :: Text -> SearchResults JobInfo -> Bool -> Int -> Bool -> Html ()
renderPage keyword results' hasNextPage nextPage isExact = do
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
          input_ [type_ "text", name_ "q", value_ keyword, class_ "p-2 border border-gray-300 rounded-lg w-full"]
          div_ [class_ "mt-2"] $ do
            label_ [class_ "inline-flex items-center"] $ do
              input_ [type_ "checkbox", name_ "exact", value_ "true", class_ "form-checkbox", if isExact then checked_ else mempty]
              span_ [class_ "ml-2 text-gray-700"] "Advanced Search"
          button_ [type_ "submit", class_ "mt-2 p-2 bg-blue-500 text-white rounded-lg hover:bg-blue-600"] "Search"
        hr_ [class_ "my-4"]

        case results' of
          NoSearch -> p_ [class_ "text-gray-700"] "Please enter a search query to start."
          SearchResults [] -> p_ [class_ "text-gray-700"] "No results found."
          SearchResults results -> do
            div_ [class_ "space-y-4"] $
              mapM_ renderJob results
            div_ [class_ "flex justify-between mt-6"] $ do
              when (nextPage > 2) $
                form_ [method_ "get", class_ "inline"] $ do
                  input_ [type_ "hidden", name_ "page", value_ (T.pack (show (nextPage - 2)))]
                  input_ [type_ "hidden", name_ "q", value_ keyword]
                  input_ [type_ "hidden", name_ "exact", value_ (if isExact then "true" else "false")]
                  button_ [type_ "submit", class_ "p-2 bg-gray-300 text-gray-700 rounded-lg hover:bg-gray-400"] "Previous Page"
              when hasNextPage $
                form_ [method_ "get", class_ "inline"] $ do
                  input_ [type_ "hidden", name_ "page", value_ (T.pack (show nextPage))]
                  input_ [type_ "hidden", name_ "q", value_ keyword]
                  input_ [type_ "hidden", name_ "exact", value_ (if isExact then "true" else "false")]
                  button_ [type_ "submit", class_ "p-2 bg-blue-500 text-white rounded-lg hover:bg-blue-600"] "Next Page"

-- Function to wrap the keyword in quotes
wrapKeyword :: Maybe Text -> Maybe Text
wrapKeyword = fmap (\k -> "\"" <> k <> "\"")

-- Database query to fetch job results based on the keyword
searchJobs :: Connection -> Maybe Text -> Int -> Int -> IO (SearchResults JobInfo, Int)
searchJobs conn wrappedKeyword limit offset = do
  let countQry = "SELECT COUNT(*) FROM job WHERE job_id IN (SELECT rowid FROM job_trace WHERE trace MATCH ?);"
      dataQry = "SELECT job_id, job_date, web_url, runner_id, runner_name, job_name, project_path \
                \FROM job WHERE job_id IN (SELECT rowid FROM job_trace WHERE trace MATCH ?) \
                \ORDER BY job_date DESC LIMIT ? OFFSET ?;"
  result <- try $ do
    totalRows <- query conn countQry (Only wrappedKeyword) :: IO [Only Int]
    rows <- query conn dataQry (wrappedKeyword, limit, offset)
    let jobs = map (\(jid, jdate, url, rid, rname, jname, path) -> JobInfo jid jdate url rid rname jname path) rows
    pure $ SearchSuccess (SearchResults jobs)

  case result of
    Left (err :: SomeException) -> pure $ SearchError $ "Database error: " <> T.pack (show err)
    Right value -> pure value

-- Scotty server for the search UI
searchUIServer :: TMVar Connection -> ScottyM ()
searchUIServer connVar = do
  get "/" $ do
    -- Get the keyword from the query parameter
    mKeyword <- (queryParamMaybe "q" :: ActionM (Maybe Text)) `rescue` (\(_ :: ScottyException) -> return Nothing)
    
    -- Get the state of the "Advanced Search" checkbox from the query parameter, default to "false" if not present
    isExactQueryParam <- (queryParamMaybe "exact" :: ActionM (Maybe Text)) `rescue` (\(_ :: ScottyException) -> return Nothing)

    -- Default to 1 if page is not provided
    page <- (queryParamMaybe "page" :: ActionM (Maybe Int)) `rescue` (\(_ :: ScottyException) -> return (Just 1))

    let isExact = case isExactQueryParam of
                    Just "true" -> True
                    _ -> False

    let keyword = maybe "" id mKeyword
        pageSize = 50
        offset = (maybe 1 id page - 1) * pageSize
        wrappedKeyword = if isExact then Just keyword else wrapKeyword (Just keyword)

    conn <- liftIO $ atomically $ readTMVar connVar

    results' <- case mKeyword of
      Nothing -> return NoSearch  -- No search performed
      Just "" -> do
        (searchResults, totalCount) <- liftIO $ searchJobs conn wrappedKeyword pageSize offset
        return searchResults  -- Perform search with empty string
      Just _ -> do
        (searchResults, totalCount) <- liftIO $ searchJobs conn wrappedKeyword pageSize offset
        return searchResults  -- Perform search with provided keyword

    let hasNextPage = case results' of
                        SearchResults jobs -> length jobs == pageSize
                        _ -> False

    html $ renderText (renderPage keyword results' hasNextPage (maybe 1 id page + 1) isExact)
