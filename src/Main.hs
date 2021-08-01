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
-- import Data.Char
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
import Data.Text as T
import Data.Text.IO
import Data.Text.Encoding as Enc
import qualified Text.Show as TS
import qualified System.Console.Terminal.Size as TermSize
import Control.Concurrent.Async

data Config = Config
    { username :: Text
    , token :: Text
    , orgName :: Text
    }
    deriving (TS.Show)

data Repository = Repository
    { name :: Text
    , url :: Text
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
    { login :: Text
    , id :: Int
    }
    deriving (Generic, Show)

data PR = PR 
    { id :: Int
    , number :: Int 
    , title :: Text
    , created_at :: UTCTime
    , repository_url :: Text
    , html_url :: Text
    , user :: PRUser
    , draft :: Bool
    , labels :: [PRLabel]
    }
    deriving (Generic, Show)

data PRLabel = PRLabel
    { name :: Text 
    }
    deriving (Generic, Show)

data PRReview = PRReview
    { user :: PRUser
    , state :: Text
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

-- requires truecolor terminal, which most support, including iterm, but
-- not built in terminal.app
-- \ESC[38;<r>;<g>;<b>;<m>m", where m is modifier (bold, italic, etc)
green :: T.Text
green = "\ESC[38;2;0;225;0;1m"
blue :: T.Text
blue = "\ESC[38;2;0;0;200;1m"
red :: T.Text
red = "\ESC[38;2;200;0;0;1m"
gray :: T.Text
gray = "\ESC[38;2;200;200;200;1m"
defaultColor :: T.Text
defaultColor = "\ESC[39m"

reset :: T.Text
reset = "\ESC[0m"
{--
 - tshow which PRs I've approved - green check
 - tshow which PRs I've commented on - green comment count
 -}
printPrForCurTime :: UTCTime 
                  -> Maybe (TermSize.Window Int) 
                  -> Map.Map PR (Int, Bool)
                  -> Map.Map PR (Int, Bool)
                  -> PR 
                  -> Text
printPrForCurTime curTime termSize commentsByPr reviewsByPr pr@PR{html_url, number, title, created_at, user=PRUser{login}, draft, labels} =
    T.pack $ printf "%s %s %s %-*s %-*s %s %-*s"
        reviewText
        commentText
        prNumberText
        timeWidth formattedTime 
        loginWidth (ellipsisTruncate loginWidth login)
        labelTags
        titleWidth (ellipsisTruncate titleWidth titleText)
    where 
        isReadyToTest PRLabel{name} = (T.toLower name) == "ready to test"
        readyToTest = (List.length $ List.filter isReadyToTest labels) > 0
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
                w - (reviewsWidth + commentWidth + prNumWidth + timeWidth + loginWidth + (T.length labelTags)) - 7
        testLabel = if readyToTest then
                        "\10004 TEST" :: Text
                    else
                        "" :: Text
        -- testLabel = if (length labels) > 0 then
        --                 intercalate " " (map (\PRLabel{name} -> name) labels)
        --             else
        --                 "" :: Text
        anchorEscapeSeq = "\27]8;;%s\27\&\\%s\27]8;;\27\\\x7"
        prNumberText = T.pack $ printf anchorEscapeSeq html_url (tshow number) :: Text
            -- "\27]8;;https://google.com/\27\&\\Link to example website\27]8;;\27\\\x7"
        formattedTime = humanDuration curTime created_at
        (commentCount, userComment) = fromMaybe (0, False) (Map.lookup pr commentsByPr)
        commentText = case (commentCount, userComment) of
                        (0, _) -> (" " :: T.Text)
                        (c, True) -> green <> tshow c <> defaultColor
                        (c, False) -> tshow c
        reviewText = case Map.lookup pr reviewsByPr of
                Nothing -> "   " :: Text
                Just (0, _) -> "   " 
                Just (1, True) -> green <> "\10004  " <> defaultColor
                Just (1, _) -> "\10004  " 
                Just (_, True) -> green <> "\10004" <> defaultColor <> "\10004 "
                Just (_, _) -> "\10004\10004 "
        labelTags = 
            blue <> (intercalate " " $ (List.map (\PRLabel{name} -> "(" <> name <> ")") labels)) <> defaultColor
        titleText = if draft == True then
                        intercalate " " ([blue <> "(DRAFT)" <> defaultColor] <> [title])
                    else
                        conflictOrDraft <> " " <> title
        conflictOrDraft = if draft then
                            blue <> "(DRAFT)" <> defaultColor
                          else
                            ""
                            -- fromMaybe "" $ fmap (\d -> if d then "" else red <> "(CONFLICTED)" <> defaultColor) mergeable


ellipsisTruncate :: Int -> Text -> Text
ellipsisTruncate n s =
    if (T.length s > n) then
        T.take (n-3) s <> "..."
    else
        s

humanDuration :: UTCTime -> UTCTime -> Text
humanDuration curTime t =
        List.head
        $ List.map (\(a,b) -> (tshow a) <> b)
        $ List.dropWhile (\(a,b) -> a == 0) [
            (days, "d"),
            (hours, "h"),
            (minutes, "m"),
            (seconds, "s")
        ]
    where
        totalSeconds :: Int
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


tshow :: Show a => a -> T.Text
tshow = T.pack . Prelude.show 
