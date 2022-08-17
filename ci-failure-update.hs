{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
import Data.List
import Data.Time
import Database.SQLite.Simple as DB

data Entry = Entry { jobId :: Int, typ :: String , date :: UTCTime, webUrl :: String, runnerId :: Maybe Int }
    deriving (Eq, Show)

instance DB.FromRow Entry where
    fromRow = Entry <$> field <*> field <*> field <*> field <*> field

main = do
    rows :: [Entry] <- DB.withConnection "jobs.db" $
        DB.query_ `flip`
            "select job_id, typ, created_at, web_url, runner_id from all_types"
    -- HMMMM Little Bobby Tables is looking on with some serious questions
    -- My answer: I control the dang source data.
    let values = map stringify rows
    putStrLn "begin; insert into ci_failure (job_id, type, job_date, web_url, runner_id) values" 
    putStrLn $ intercalate ",\n" values
    putStrLn "on conflict (job_id, type) do update set runner_id = excluded.runner_id where ci_failure.runner_id is null; commit"


stringify :: Entry -> String
stringify (Entry jid ty dat url pid) = "(" <> intercalate ", " lst <> ")" where
    lst = [show jid, squote ty, squote (show dat), squote url, mInt pid]
    mInt = maybe "null" show
    squote x = "'" <> x <> "'"
