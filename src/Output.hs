{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Output where

import Data.Maybe
import qualified Data.List as List
import Data.Time.Clock
import Data.Text as T
import Data.Text.IO as T
import Text.Printf
import qualified Data.Map.Strict as Map
import qualified System.Console.Terminal.Size as TermSize
import System.Console.ANSI
import Data.Colour.SRGB
import Control.Monad
import Debug.Trace

import Types

-- requires truecolor terminal, which most support, including iterm, but
-- not built in terminal.app
-- \ESC[38;<r>;<g>;<b>;<m>m", where m is modifier (bold, italic, etc)
green = "\ESC[38;2;0;225;0;1m"
blue = "\ESC[38;2;0;0;200;1m"
red = "\ESC[38;2;200;0;0;1m"
gray = "\ESC[38;2;200;200;200;1m"
defaultColor = "\ESC[39m"

checkmark = "\10004"

_green :: Colour Float
_green = sRGB (0.0 :: Float) 100.0 0.0

-- data PrintCmd = ColorCmd (Colour Float) T.Text | NoColorCmd T.Text
-- data PrintPadding = PadLeft Int | PadRight Int | NoPadding
--


printGroupedPRs :: UTCTime 
                -> Maybe (TermSize.Window Int) 
                -> Map.Map Text Text
                -> Map.Map PR (Int, Bool)
                -> Map.Map Int PRReviewSummary
                -> [[PR]] 
                -> IO ()
printGroupedPRs curTime termSize reposByUrl commentsByPr reviewsByPr groupedPRs = do
    let withRepoName = List.map (\prs -> (fromMaybe "???" $ Map.lookup (repoUrl prs) reposByUrl, prs)) groupedPRs
    forM_ withRepoName (\(repoName, prs) -> do
        T.putStrLn repoName
        T.putStrLn "--------"

        let prsAsTextChunks = fmap (\pr@PR{number} -> do
                printPrForCurTime curTime termSize commentsByPr (fromMaybe (PRReviewSummary 0 False) (Map.lookup number reviewsByPr)) pr
                ) prs :: [[Text]]
        let columnized = columnize prsAsTextChunks
        T.putStrLn $ T.unlines columnized
        )


repoUrl [] = ""
repoUrl prs = repository_url $ List.head prs

columnize :: [[Text]] -> [Text]
columnize prChunkSets =
    List.map (\prChunks -> T.intercalate " " $ List.zipWith (\chunk width -> padRight width chunk) prChunks colWidths) prChunkSets
    where
        colWidths :: [Int]
        colWidths = 
            List.foldr (\prChunks result -> mapZipAll lengthWithoutColor max prChunks result ) ([]:: [Int]) prChunkSets

mapZipAll :: (a -> b) -> (b->b->b) -> [a]->[b]->[b]
mapZipAll f g = go
  where
    go [] [] = []
    go [] (y:ys) = y : go [] ys
    go (x:xs) [] = f x : go xs []
    go (x:xs) (y:ys) = g (f x) (y) : go xs ys

printPrForCurTime :: UTCTime 
                  -> Maybe (TermSize.Window Int) 
                  -> Map.Map PR (Int, Bool)
                  -> PRReviewSummary
                  -> PR 
                  -> [Text]
printPrForCurTime curTime termSize commentsByPr PRReviewSummary{reviewCount, curUserApproved} pr@PR{html_url, number, title, created_at, user=PRUser{login}, draft, labels} =
    fmap T.strip
        [ reviewText
        , commentText
        , prNumberText
        , formattedTime
        , (ellipsisTruncate loginWidth login)
        , labelTags
        , (ellipsisTruncate titleWidth titleText)
        ]
    where 
        isReadyToTest PRLabel{name} = (T.toLower name) == "ready to test"
        readyToTest = (List.length $ List.filter isReadyToTest labels) > 0
                
        loginWidth = 15
        titleWidth = case termSize of
            Nothing -> 50
            Just TermSize.Window{TermSize.width=w} -> 
                w - (reviewsWidth + commentWidth + prNumWidth + timeWidth + loginWidth + (T.length labelTags)) - 7
        anchorEscapeSeq = "\27]8;;%s\27\&\\%s\27]8;;\27\\\x7"
        prNumberText = T.pack $ printf anchorEscapeSeq html_url (tshow number) :: Text
            -- "\27]8;;https://google.com/\27\&\\Link to example website\27]8;;\27\\\x7"
        formattedTime = humanDuration curTime created_at
        (commentCount, userComment) = fromMaybe (0, False) (Map.lookup pr commentsByPr)
        commentText = case (commentCount, userComment) of
                        (0, _) -> (" " :: T.Text)
                        (c, True) -> green <> tshow c <> defaultColor
                        (c, False) -> tshow c
        reviewText = case (reviewCount, curUserApproved) of
                (0, _) -> "   " 
                (1, True) -> green <> "\10004  " <> defaultColor
                (1, _) -> "\10004  " 
                (_, True) -> green <> "\10004" <> defaultColor <> "\10004 "
                (_, _) -> "\10004\10004 "
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

padLeft :: Int -> Text -> Text
padLeft n txt =
    (T.replicate padCount " ") <> txt
    where
        padCount = n - (lengthWithoutColor txt)

padRight :: Int -> Text -> Text
padRight n txt =
    result
    where
        result = txt <> (T.replicate padCount " ")
        padCount = n - lenWithoutColor
        lenWithoutColor = lengthWithoutColor txt

tshow :: Show a => a -> T.Text
tshow = T.pack . Prelude.show 

lengthWithoutColor :: Text -> Int
lengthWithoutColor txt =
    T.length $ T.replace green "" $ T.replace red "" $ T.replace defaultColor "" txt

