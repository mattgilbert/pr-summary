{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Debug.Trace
import System.Exit
import System.Directory (doesFileExist, getHomeDirectory)
import System.FilePath ((</>))
import Data.List
import Data.Maybe
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Base64 as Base64
import Network.HTTP.Simple
import GHC.Generics
import Data.Aeson (FromJSON(..))
import Data.Time.Clock
import Data.Map.Strict as Map

-- TODO: instead of passing this around and using it, can
-- we add a request function to this that's ready-made to
-- be used with the appropriate headers, etc?
data Config = Config
    { username :: String
    , token :: String
    , orgName :: String
    }
    deriving (Show)

data Repository = Repository
    { name :: String
    , url :: String
    }
    deriving (Generic, Show)

instance FromJSON Repository

data PRList = PRList
    { total_count :: Int
    , incomplete_results :: Bool
    , items :: [PR]
    }
    deriving (Generic, Show)

data PR = PR 
    { number :: Int 
    , title :: String
    , created_at :: UTCTime
    , repository_url :: String
    }
    deriving (Generic, Show)

instance FromJSON PRList
instance FromJSON PR

main :: IO ()
main = do
    config <- getConfig
    reposByUrl <- getRepositories config
    openPRs <- getOpenPRs config

    let sortByRepoUrl = sortBy comparePrRepoUrls
    let groupByRepoUrl = groupBy sameRepo
    let groupedPRs = (groupByRepoUrl . sortByRepoUrl) openPRs

    let groupStrings = fmap (groupLines reposByUrl) groupedPRs
    putStrLn $ unlines $ concat groupStrings

    where
        -- TODO: this is almost criminal, rework this
        groupLines reposByUrl grp = 
            ["", (fromJust $ Map.lookup (repository_url $ head grp) reposByUrl), "------"] ++
            (fmap (\PR{number, title} -> (show number) ++ " - " ++ title) grp)
    
comparePrRepoUrls :: PR -> PR -> Ordering
comparePrRepoUrls PR{repository_url=url1} PR{repository_url=url2} =
    compare url1 url2

sameRepo :: PR -> PR -> Bool
sameRepo PR{repository_url=url1} PR{repository_url=url2} =
    url1 == url2

getConfig :: IO Config
getConfig = do
    homePath <- getHomeDirectory
    let tokenFile = homePath </> ".pr-summary"

    exists <- doesFileExist tokenFile
    authInfo <- if not exists then do
                   print "Missing token file ~/.pr-summary"
                   exitFailure
                else do
                   readFile tokenFile

    let newlineChar = 10
    -- TODO: too fragile, rework this
    let username:token:orgName:_ = lines authInfo
    pure $ Config username token orgName

getAuthHeader :: Config -> String
getAuthHeader Config{username, token} =
    BS8.unpack $ "Basic " <> (Base64.encode $ (BS8.pack username) <> ":" <> (BS8.pack token))

getRepositories :: Config -> IO (Map String String)
getRepositories config@Config{username, token, orgName} = do
    let authHeader = getAuthHeader config
    initReq <- parseRequest $ "https://api.github.com/orgs/" <> orgName <> "/repos"
    let request = 
            addRequestHeader "user-agent" (BS8.pack username) $
            addRequestHeader "Authorization" (BS8.pack authHeader) $
            initReq

    responseAttempt <- httpJSONEither request :: IO (Response (Either JSONException [Repository]))

    case (getResponseBody responseAttempt) of
        Left err -> do
            case err of
                JSONParseException _ _ _ -> do
                    print "parse exception"
                JSONConversionException _ _ _ -> do
                    print "parse exception"
            exitFailure
        Right repos -> do
            let pairs = fmap (\Repository{name,url} -> (url, name)) repos
            pure $ fromList pairs

getOpenPRs :: Config -> IO [PR]
getOpenPRs config@Config{username, token, orgName} = do
    let authHeader = getAuthHeader config
    let searchQuery = intercalate " "
            [ "org:" <> orgName
            , "type:pr" 
            , "state:open"
            ]
    let query = [("q", Just (BS8.pack searchQuery))]
    initReq <- parseRequest "https://api.github.com/search/issues"

    let request = 
            addRequestHeader "user-agent" (BS8.pack username) $
            addRequestHeader "Authorization" (BS8.pack authHeader) $
            setRequestQueryString query $
            initReq

    responseAttempt <- httpJSONEither request :: IO (Response (Either JSONException PRList))

    case (getResponseBody responseAttempt) of
        Left err -> do
            case err of
                JSONParseException _ _ _ ->
                    print "parse exception"
                JSONConversionException _ _ _ ->
                    print "parse exception"
            exitFailure
        Right PRList{total_count, items, incomplete_results} ->
            pure items


formatPRs :: [PR] -> String
formatPRs prs = 
    unlines prStrList
    where
    prStrList = (\PR{number, title} -> intercalate " " [ (show number) , title ]) <$> prs
    
{--
 Next:
 x start by simple request to api.github.com
 x need to use username as user-agent
 x check reponse code
 x do a cross org PR search
 x get a repo list by url, so we can group PRs
 x use ~/.pr-summary to store username/access token
 x use ~/.pr-summary to store a default org 
 - CODE CLEANUP!!!
 - order by oldest for each group
 - clean up and abstract duplicate code for http requests
 - truncate long descriptions
 - show created time, show author, create fixed width columns
 - show comments count

 Functionality goals:
 - list all open PRs, sorted by oldest->newest
 - show which PRs have comments (by me vs others)
   - which PRs have comments by me without reply
 - show which PRs are mine
 - show which PRs have 2 approvals
 - consider some kind of oauth thing from the command line
 -}
