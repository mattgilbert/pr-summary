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
import Data.Char
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
    , id :: Int
    }
    deriving (Generic, Show)

data PR = PR 
    { id :: Int
    , number :: Int 
    , title :: String
    , created_at :: UTCTime
    , repository_url :: String
    , html_url :: String
    , user :: PRUser
    , draft :: Bool
    , labels :: [PRLabel]
    }
    deriving (Generic, Show)

data PRLabel = PRLabel
    { name :: String 
    }
    deriving (Generic, Show)

data PRReview = PRReview
    { user :: PRUser
    , state :: String
    }
    deriving (Generic, Show)

data PRComment = PRComment
    { id :: Int
    , user :: PRUser
    }
    deriving (Generic, Show)

instance FromJSON PRReview
instance FromJSON PRComment
instance FromJSON PRUser
instance FromJSON PRList
instance FromJSON PR
instance FromJSON PRLabel

instance Eq PR where
    PR{id=id1} == PR{id=id2} = id1 == id2
instance Ord PR where
    compare PR{id=id1} PR{id=id2} = compare id1 id2

anchor = 
    putStrLn link
    where
    -- fmt = "\27]8;;%s\27\&\\%s\27]8;;\27\\\x7"
    fmt = "aaa %s bbb"
    link = printf fmt ("https://google.com"::String) ("link text"::String) -- url (show number)

    
main :: IO ()
main = do
    curTime <- getCurrentTime
    termSize <- TermSize.size
    config@Config{username=curUsername} <- getConfig

    putStrLn "Loading PRs and repositories concurrently..."
    repoRequest <- async $ getRepositories config
    prRequest <- async $ getOpenPRs config
    reposByUrl <- wait repoRequest
    openPRs <- wait prRequest

    putStrLn "Loading comments concurrently..."
    commentCounts <- mapConcurrently (getPRComments config) openPRs
    let commentCountsByPr = Map.fromList commentCounts

    putStrLn "Loading reviews/approvals concurrently..."
    reviewCounts <- mapConcurrently (getPRReviewCount curUsername config) openPRs
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
        repoName = maybe "???" (\x -> x) maybeRepoName
        separator = replicate (length repoName) '-'

-- requires truecolor terminal, which most support, including iterm, but
-- not built in terminal.app
-- \ESC[38;<r>;<g>;<b>;<m>m", where m is modifier (bold, italic, etc)
green = "\ESC[38;2;0;225;0;1m"
blue = "\ESC[38;2;0;0;200;1m"
red = "\ESC[38;2;200;0;0;1m"
gray = "\ESC[38;2;200;200;200;1m"
defaultColor = "\ESC[39m"
reset = "\ESC[0m"
{--
 - show which PRs I've approved - green check
 - show which PRs I've commented on - green comment count
 -}
printPrForCurTime :: UTCTime 
                  -> Maybe (TermSize.Window Int) 
                  -> Map.Map PR (Int, Bool)
                  -> Map.Map PR (Int, Bool)
                  -> PR 
                  -> String
printPrForCurTime curTime termSize commentsByPr reviewsByPr pr@PR{html_url, number, title, created_at, user=PRUser{login}, draft, labels} =
    printf "%s %s %s %-*s %-*s %s %-*s"
        reviewText
        commentText
        prNumberText
        timeWidth formattedTime 
        loginWidth (ellipsisTruncate loginWidth login)
        labelTags
        titleWidth (ellipsisTruncate titleWidth titleText)
    where 
        readyToTest = (length $ filter (\PRLabel{name} -> (map toLower name) == "ready to test") labels) > 0
        -- rowColor = if readyToTest then gray else defaultColor
                
        reviewsWidth = 2
        commentWidth = 2
        prNumWidth = 5
        timeWidth = 3
        loginWidth = 15
        testLabelWidth = 7
        titleWidth = case termSize of
            Nothing -> 50
            Just TermSize.Window{TermSize.width=w} -> 
                w - (reviewsWidth + commentWidth + prNumWidth + timeWidth + loginWidth + (length labelTags)) - 7
        testLabel = if readyToTest then
                        "\10004 TEST" :: String
                    else
                        "" :: String
        -- testLabel = if (length labels) > 0 then
        --                 intercalate " " (map (\PRLabel{name} -> name) labels)
        --             else
        --                 "" :: String
        anchorEscapeSeq = "\27]8;;%s\27\&\\%s\27]8;;\27\\\x7"
        prNumberText = printf anchorEscapeSeq html_url (show number) :: String
            -- "\27]8;;https://google.com/\27\&\\Link to example website\27]8;;\27\\\x7"
        formattedTime = humanDuration curTime created_at
        (commentCount, userComment) = fromMaybe (0, False) (Map.lookup pr commentsByPr)
        commentText = case (commentCount, userComment) of
                        (0, _) -> " "
                        (c, True) -> green <> show c <> defaultColor
                        (c, False) -> show c
        reviewText = case Map.lookup pr reviewsByPr of
                Nothing -> "   " :: String
                Just (0, _) -> "   " 
                Just (1, True) -> green <> "\10004  " <> defaultColor
                Just (1, _) -> "\10004  " 
                Just (_, True) -> green <> "\10004" <> defaultColor <> "\10004 "
                Just (_, _) -> "\10004\10004 "
        labelTags = 
            blue <> (intercalate " " $ (map (\PRLabel{name} -> "(" <> name <> ")") labels)) <> defaultColor
        titleText = if draft == True then
                        intercalate " " ([blue <> "(DRAFT)" <> defaultColor] <> [title])
                    else
                        conflictOrDraft <> " " <> title
        conflictOrDraft = if draft then
                            blue <> "(DRAFT)" <> defaultColor
                          else
                            ""
                            -- fromMaybe "" $ fmap (\d -> if d then "" else red <> "(CONFLICTED)" <> defaultColor) mergeable


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

getRepositories :: Config -> IO (Map.Map String String)
getRepositories config@Config{username, token, orgName} = do
    let url = printf "https://api.github.com/orgs/%s/repos" orgName
    repos <- httpGet config url

    let pairs = fmap (\Repository{name,url} -> (url, name)) repos
    pure $ Map.fromList pairs

getOpenPRs :: Config -> IO [PR]
getOpenPRs config@Config{username, token, orgName} = do
    let url = "https://api.github.com/search/issues"
    let searchQuery = intercalate " "
            [ "org:" <> orgName
            , "type:pr" 
            , "state:open"
            ]
    let query = [ ("q", Just (BS8.pack searchQuery))
                , ("per_page", Just "100")
                ]
    PRList{items} <- httpGetQuery config url query :: IO PRList

    pure items


getPRComments :: Config -> PR -> IO (PR, (Int, Bool))
getPRComments config@Config{orgName, username, token} pr@PR{number, repository_url=repoUrl} = do
    let url = printf "%s/pulls/%d/comments" repoUrl number
    comments <- httpGet config url :: IO [PRComment]

    let Config{username=curUsername} = config
    let uniqueCommenters = nub $ 
            map (\PRComment{user=PRUser{login}} -> login) comments

    let curUserCommented = curUsername `elem` uniqueCommenters

    pure (pr, (length comments, curUserCommented))

getPRReviewCount :: String -> Config -> PR -> IO (PR, (Int, Bool))
getPRReviewCount curUsername config@Config{orgName, username, token} pr@PR{number, repository_url=repoUrl} = do
    let url = printf "%s/pulls/%d/reviews" repoUrl number
    reviews <- httpGet config url :: IO [PRReview]

    let approvalCount = length $ nub $ 
            map (\PRReview{user=PRUser{login}} -> login) $ 
            filter (\PRReview{state} -> state == "APPROVED") reviews

    let uniqueApprovals = nub $ 
            map (\PRReview{user=PRUser{login}} -> login) $ -- one user might have multiple reviews
            filter (\PRReview{state} -> state == "APPROVED") reviews

    let curUserApproved = curUsername `elem` uniqueApprovals

    pure (pr, (approvalCount, curUserApproved))

httpGet :: FromJSON a => Config -> String -> IO a
httpGet config url =
    httpGetQuery config url []

httpGetQuery :: FromJSON a => Config -> String -> [(BS8.ByteString,Maybe BS8.ByteString)] -> IO a
httpGetQuery config@Config{orgName, username, token} url query = do
    let authHeader = getAuthHeader config
    initReq <- parseRequest url

    let request = 
            addRequestHeader "user-agent" (BS8.pack username) $
            addRequestHeader "Authorization" (BS8.pack authHeader) $
            initReq

    let queryReq = 
            if length query > 0 then
                setRequestQueryString query request
            else
                request

    responseAttempt <- httpJSONEither queryReq -- :: IO (Response (Either JSONException a))

    case (getResponseBody responseAttempt) of
        Left err -> do
            case err of
                JSONParseException _ _ _ ->
                    print "parse exception"
                JSONConversionException _ _ _ ->
                    print err
            exitFailure
        Right result -> 
            pure result


getAuthHeader :: Config -> String
getAuthHeader Config{username, token} =
    BS8.unpack $ "Basic " <> (Base64.encode $ (BS8.pack username) <> ":" <> (BS8.pack token))
