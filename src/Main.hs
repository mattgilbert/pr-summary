{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Prelude hiding (id, unlines, lines, readFile, putStrLn)
import Debug.Trace
import System.Exit
import System.Directory (doesFileExist, getHomeDirectory)
import System.FilePath ((</>))
import qualified Data.List as List
import Data.Maybe
import Data.Ord
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Base64 as Base64
import Network.HTTP.Simple
import Data.Aeson (FromJSON(..))
import Data.Time.Format
import Data.Time.LocalTime
import Data.Time.Clock
import Data.Time.Clock.POSIX
import qualified Data.Map.Strict as Map
import Text.Printf
import Data.Text as T
import qualified Text.Show as T
import Data.Text.IO
import Data.Text.Encoding as Enc
import Control.Concurrent.Async
import qualified System.Console.Terminal.Size as TermSize

import Types
import Output

anchor = 
    putStrLn link
    where
    -- fmt = "\27]8;;%s\27\&\\%s\27]8;;\27\\\x7"
    fmt = "aaa %s bbb"
    link = T.pack $ printf fmt ("https://google.com"::Text) ("link text"::Text) -- url (tshow number)

    
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
            [""] <> (formatRepoName reposByUrl grp) <> (fmap printPR (List.sortBy comparePrDate grp))

    let sortByRepoUrl = List.sortBy comparePrRepoUrls
    let groupByRepoUrl = List.groupBy sameRepo
    let groupedPRs = (groupByRepoUrl . sortByRepoUrl) openPRs

    let groupTexts = fmap (groupLines reposByUrl) groupedPRs
    putStrLn $ unlines $ List.concat groupTexts


formatRepoName :: (Map.Map Text Text) -> [PR] -> [Text]
formatRepoName reposByUrl grp =
    [repoName, separator]
    where
        url = repository_url $ List.head grp
        maybeRepoName = Map.lookup url reposByUrl
        repoName = maybe "???" (\x -> x) maybeRepoName
        separator = T.replicate (T.length repoName) "-"

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
    let username:token:orgName:_ = T.lines authInfo
    pure $ Config username token orgName

getRepositories :: Config -> IO (Map.Map Text Text)
getRepositories config@Config{username, token, orgName} = do
    let url = T.pack $ printf "https://api.github.com/orgs/%s/repos" orgName
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
    let query = [ ("q", Just (Enc.encodeUtf8 searchQuery))
                , ("per_page", Just "100")
                ]
    PRList{items} <- httpGetQuery config url query :: IO PRList

    pure items


getPRComments :: Config -> PR -> IO (PR, (Int, Bool))
getPRComments config@Config{orgName, username, token} pr@PR{number, repository_url=repoUrl} = do
    let url = T.pack $ printf "%s/pulls/%d/comments" repoUrl number
    comments <- httpGet config url :: IO [PRComment]

    let Config{username=curUsername} = config
    let uniqueCommenters = List.nub $ 
            List.map (\PRComment{user=PRUser{login}} -> login) comments

    let curUserCommented = curUsername `elem` uniqueCommenters

    pure (pr, (List.length comments, curUserCommented))

getPRReviewCount :: Text -> Config -> PR -> IO (PR, (Int, Bool))
getPRReviewCount curUsername config@Config{orgName, username, token} pr@PR{number, repository_url=repoUrl} = do
    let url = T.pack $ printf "%s/pulls/%d/reviews" repoUrl number
    reviews <- httpGet config url :: IO [PRReview]

    let approvalCount = List.length $ List.nub $ 
            List.map (\PRReview{user=PRUser{login}} -> login) $ 
            List.filter (\PRReview{state} -> state == "APPROVED") reviews

    let uniqueApprovals = List.nub $ 
            List.map (\PRReview{user=PRUser{login}} -> login) $ -- one user might have multiple reviews
            List.filter (\PRReview{state} -> state == "APPROVED") reviews

    let curUserApproved = curUsername `elem` uniqueApprovals

    pure (pr, (approvalCount, curUserApproved))

httpGet :: FromJSON a => Config -> Text -> IO a
httpGet config url =
    httpGetQuery config url []

httpGetQuery :: FromJSON a => Config -> Text -> [(BS8.ByteString,Maybe BS8.ByteString)] -> IO a
httpGetQuery config@Config{orgName, username, token} url query = do
    let authHeader = getAuthHeader config
    initReq <- parseRequest $ T.unpack url

    let request = 
            addRequestHeader "user-agent" (Enc.encodeUtf8 username) $
            addRequestHeader "Authorization" (Enc.encodeUtf8 authHeader) $
            initReq

    let queryReq = 
            if List.length query > 0 then
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


getAuthHeader :: Config -> Text
getAuthHeader Config{username, token} =
    Enc.decodeUtf8 $ "Basic " <> (Base64.encode $ (Enc.encodeUtf8 username) <> ":" <> (Enc.encodeUtf8 token))

