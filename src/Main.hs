{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Prelude hiding (id)
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
import Control.Concurrent.Async

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
    { id :: Int
    , number :: Int 
    , title :: String
    , created_at :: UTCTime
    , repository_url :: String
    , user :: PRUser
    }
    deriving (Generic, Show)

data PRReview = PRReview
    { user :: PRUser
    , state :: String
    }
    deriving (Generic, Show)

data PRItem = PRItem
    { id :: Int
    }
    deriving (Generic, Show)

instance FromJSON PRReview
instance FromJSON PRItem
instance FromJSON PRUser
instance FromJSON PRList
instance FromJSON PR

instance Eq PR where
    PR{id=id1} == PR{id=id2} = id1 == id2
instance Ord PR where
    compare PR{id=id1} PR{id=id2} = compare id1 id2

main :: IO ()
main = do
    curTime <- getCurrentTime
    termSize <- TermSize.size
    config <- getConfig

    putStrLn "Loading PRs and repositories concurrently..."
    repoRequest <- async $ getRepositories config
    prRequest <- async $ getOpenPRs config
    reposByUrl <- wait repoRequest
    openPRs <- wait prRequest

    putStrLn "Loading comments concurrently..."
    commentCounts <- mapConcurrently (getPRItemCount config) openPRs
    let commentCountsByPr = Map.fromList commentCounts

    putStrLn "Loading reviews/approvals concurrently..."
    reviewCounts <- mapConcurrently (getPRReviewCount config) openPRs
    let reviewCountsByPr = Map.fromList reviewCounts

    let printPR = printPrForCurTime curTime termSize commentCountsByPr reviewCountsByPr
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


printPrForCurTime :: UTCTime -> Maybe (TermSize.Window Int) -> Map.Map PR Int -> Map.Map PR Int -> PR -> String
printPrForCurTime curTime termSize commentsByPr reviewsByPr pr@PR{number, title, created_at, user=PRUser{login}} =
    printf "%-*s %*s %*s %-*s %-*s %-*s"
        reviewsWidth reviewText
        commentWidth commentText
        prNumWidth prNumberText
        timeWidth formattedTime 
        loginWidth (ellipsisTruncate loginWidth login)
        titleWidth (ellipsisTruncate titleWidth title)
    where 
        reviewsWidth = 2
        commentWidth = 3
        prNumWidth = 5
        timeWidth = 3
        loginWidth = 15
        titleWidth = case termSize of
            Nothing -> 50
            Just TermSize.Window{TermSize.width=w} -> 
                w - (reviewsWidth + commentWidth + prNumWidth + timeWidth + loginWidth) - 7
        prNumberText = (show number) -- "\27[8;;https://example.com/^GLink to example website^[]8;;\x7" :: String
        formattedTime = humanDuration curTime created_at
        commentCount = fromMaybe 0 (Map.lookup pr commentsByPr)
        commentText = 
            if commentCount > 0 then
                (show commentCount) <> "\128172" -- speech balloon
            else
                ""
        reviewText = case (Map.lookup pr reviewsByPr) of
                Nothing -> " " :: String
                Just 0 -> " " 
                Just 1 -> "\10004" 
                Just _ -> "\10004\10004"

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
                   putStrLn "Missing token file ~/.pr-summary"
                   exitFailure
                else do
                   readFile tokenFile

    let newlineChar = 10
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


getPRItemCount :: Config -> PR -> IO (PR, Int)
getPRItemCount config@Config{orgName, username, token} pr@PR{number, repository_url=repoUrl} = do
    let authHeader = getAuthHeader config
    initReq <- parseRequest $ printf "%s/pulls/%d/comments" repoUrl number

    let request = 
            addRequestHeader "user-agent" (BS8.pack username) $
            addRequestHeader "Authorization" (BS8.pack authHeader) $
            initReq

    responseAttempt <- httpJSONEither request :: IO (Response (Either JSONException [PRItem]))

    case (getResponseBody responseAttempt) of
        Left err -> do
            case err of
                JSONParseException _ _ _ ->
                    print "parse exception"
                JSONConversionException _ _ _ ->
                    print err
            exitFailure
        Right comments ->
            pure $ (pr, length comments)

getPRReviewCount :: Config -> PR -> IO (PR, Int)
getPRReviewCount config@Config{orgName, username, token} pr@PR{number, repository_url=repoUrl} = do
    let authHeader = getAuthHeader config
    initReq <- parseRequest $ printf "%s/pulls/%d/reviews" repoUrl number

    let request = 
            addRequestHeader "user-agent" (BS8.pack username) $
            addRequestHeader "Authorization" (BS8.pack authHeader) $
            initReq

    responseAttempt <- httpJSONEither request :: IO (Response (Either JSONException [PRReview]))

    case (getResponseBody responseAttempt) of
        Left err -> do
            case err of
                JSONParseException _ _ _ ->
                    print "parse exception"
                JSONConversionException _ _ _ ->
                    print err
            exitFailure
        Right reviews -> 
            pure $ (pr, approvalCount)
            where
                approvalCount =
                    length $ nub $ map (\PRReview{user=PRUser{login}} -> login) $
                    filter (\PRReview{state} -> state == "APPROVED") reviews

{--
 Next:
 x start by simple request to api.github.com
 x need to use username as user-agent
 x check reponse code
 x do a cross org PR search
 x get a repo list by url, so we can group PRs
 x use ~/.pr-summary to store username/access token
 x use ~/.pr-summary to store a default org 
 x order by oldest for each group
 x truncate long descriptions
 x show created time, show author, create fixed width columns
 x parallelize comments/review http requests
 x column format
   approvals comment count PR# author age comment-count desc
   x get terminal width to determine width of pr desc
   x comment count: 3💬, or if you have an unresponded comment: 3💬!, or consider coloring
   x approvals: ✔︎✔︎
 - coloring:
   - green checks when 2 approvals
   - if not approved, gray out my PRs
   - unresponded comments
 - clickable link for PR number to open github
 - code cleanup
 -}
