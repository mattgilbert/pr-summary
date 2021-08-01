{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Output where

import Data.Maybe
import qualified Data.List as List
import Data.Time.Clock
import Data.Text as T
import Text.Printf
import qualified Data.Map.Strict as Map
import qualified System.Console.Terminal.Size as TermSize

import Types

-- requires truecolor terminal, which most support, including iterm, but
-- not built in terminal.app
-- \ESC[38;<r>;<g>;<b>;<m>m", where m is modifier (bold, italic, etc)
green = "\ESC[38;2;0;225;0;1m"
blue = "\ESC[38;2;0;0;200;1m"
red = "\ESC[38;2;200;0;0;1m"
gray = "\ESC[38;2;200;200;200;1m"
defaultColor = "\ESC[39m"
reset = "\ESC[0m"


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


tshow :: Show a => a -> T.Text
tshow = T.pack . Prelude.show 
