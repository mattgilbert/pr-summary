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
import Data.Ord
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Base64 as Base64
import Network.HTTP.Simple
import GHC.Generics
import Data.Aeson (FromJSON(..))
import Data.Time.Format
import Data.Time.LocalTime
import Data.Time.Clock
import Data.Time.Clock.POSIX
import qualified Data.Map.Strict as Map
import Text.Printf
import qualified System.Console.Terminal.Size as TermSize

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

data PRUser = PRUser
    { login :: String
    }
    deriving (Generic, Show)

data PR = PR 
    { number :: Int 
    , title :: String
    , created_at :: UTCTime
    , repository_url :: String
    , user :: PRUser
    }
    deriving (Generic, Show)

instance FromJSON PRUser
instance FromJSON PRList
instance FromJSON PR

main :: IO ()
main = do
    curTime <- getCurrentTime
    termSize <- TermSize.size
    print termSize
    config <- getConfig
    reposByUrl <- getRepositories config
    openPRs <- getOpenPRs config

    let printPR = printPrForCurTime curTime termSize
    let groupLines reposByUrl grp = 
            [""] <> (formatRepoName reposByUrl grp) <> (fmap printPR (sortBy comparePrDate grp))

    let sortByRepoUrl = sortBy comparePrRepoUrls
    let groupByRepoUrl = groupBy sameRepo
    let groupedPRs = (groupByRepoUrl . sortByRepoUrl) openPRs

    let groupStrings = fmap (groupLines reposByUrl) groupedPRs
    putStrLn $ unlines $ concat groupStrings


formatRepoName :: (Map.Map String String) -> [PR] -> [String]
formatRepoName reposByUrl grp =
    [repoName, separator]
    where
        url = repository_url $ head grp
        maybeRepoName = Map.lookup url reposByUrl
        repoName = fromJust maybeRepoName
        separator = replicate (length repoName) '-'


printPrForCurTime :: UTCTime -> Maybe (TermSize.Window Int) -> PR -> String
printPrForCurTime curTime termSize PR{number, title, created_at, user=PRUser{login}} =
    printf "%-*s %-*s %-*s %-*s" 
        prNumSize (show number) 
        timeSize formattedTime 
        loginSize (ellipsisTruncate loginSize login)
        titleSize (ellipsisTruncate titleSize title)
    where 
        prNumSize = 4
        timeSize = 3
        loginSize = 15
        titleSize = case termSize of
            Nothing -> 50
            Just TermSize.Window{TermSize.width=w} -> 
                w - (prNumSize + timeSize + loginSize) - 5
        formattedTime = humanDuration curTime created_at
        --formattedTime = formatTime defaultTimeLocale "%FT%T%QZ" created_at

ellipsisTruncate :: Int -> String -> String
ellipsisTruncate n s =
    if (length s > n) then
        take (n-3) s <> "..."
    else
        s

humanDuration :: UTCTime -> UTCTime -> String
humanDuration curTime t =
        head
        $ map (\(a,b) -> (show a)++b)
        $ dropWhile (\(a,b) -> a == 0) [
            (days, "d"),
            (hours, "h"),
            (minutes, "m"),
            (seconds, "s")
        ]
    where
        totalSeconds = round $ diffUTCTime curTime t
        days = totalSeconds `div` 86400
        hours = (totalSeconds `mod` 86400) `div` 3600
        minutes = (totalSeconds `mod` 86400 `mod` 3600) `div` 60
        seconds = (totalSeconds `mod` 86400 `mod` 3600 `mod` 60)

comparePrRepoUrls :: PR -> PR -> Ordering
comparePrRepoUrls PR{repository_url=url1} PR{repository_url=url2} =
    compare url1 url2

comparePrDate :: PR -> PR -> Ordering
comparePrDate PR{created_at=created1} PR{created_at=created2} =
    compare created1 created2

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

getRepositories :: Config -> IO (Map.Map String String)
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
            pure $ Map.fromList pairs

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
 - show which ones i've approved
   - has to be queried by each PR, so run parallel requests
 - order by oldest for each group
 - column format
   PR# author age comment-count desc
   - get terminal width to determine width of pr desc
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
