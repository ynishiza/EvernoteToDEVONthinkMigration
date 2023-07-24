{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}
module Content (
  baseFontSize,
  normalizeTextFont,
  isNoteContentData,
  textFontFamily,
  horizontalLine,
  cleanseText,
  testPrintTags,
  isTextFont,
) where

import Data.Foldable
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import GHC.Exts (IsString)
import Text.HTML.TagSoup
import Text.Regex.TDFA hiding (matchCount)
import Utils

baseFontSize :: IsString s => s
baseFontSize = "14px"

{-

    <note><title>Cheatsheet: Markdown</title><content><![CDATA[<!DOCTYPE en-note SYSTEM "http://xml.evernote.com/pub/enml2.dtd"><en-note> ...

    <note><title>Cheatsheet [.NET]: cheatsheet</title><content><![CDATA[<?xml version="1.0" encoding="UTF-8" standalone="no"?><!DOCTYPE en-note SYSTEM "http://xml.evernote.com/pub/enml2.dtd">
-}
isNoteContentData :: Text -> Bool
isNoteContentData text = "<!DOCTYPE en-note" `T.isPrefixOf` text' || "<?xml version" `T.isPrefixOf` text'
 where
  text' = T.strip text

isTextFont :: Text -> Bool
isTextFont text = text =~ textFontRegex

textFontRegex :: Text
textFontRegex = T.intercalate "|" (opts <> (T.toLower <$> opts))
 where
  opts = (\x -> "(" <> x <> ")") <$> existingTextFonts

existingTextFonts :: [Text]
existingTextFonts =
  -- IMPORTANT: longer names should be first in order to make sure they get replaced first
  [ "Helvetica Neue"
  , "Helvetica"
  , "Arial Black"
  , "Arial"
  , "arial"
  , "Courier New"
  , "Courier"
  , "UICTFontTextStyleBody"
  , "Times New Roman"
  , "Times"
  , "Verdana"
  , "sans-serif"
  ]

normalizeTextFont :: Text -> Text
normalizeTextFont text = replaceTextWithRegex textFontRegex textFontFamily text

textFontFamily :: IsString s => s
textFontFamily = "Helvetica Neue"

horizontalLine :: [EvernoteTag]
horizontalLine =
  [ TagOpen "div" [("style", "text-align: center;")]
  , TagText "=============================================================="
  , TagClose "div"
  ]

cleanseText :: Text -> Text
cleanseText text = foldr (uncurry T.replace) text toReplace
 where
  toReplace =
    [ -- Double quotes
      ("“", "\"")
    , ("”", "\"")
    , -- Single quotes
      ("‘", "'")
    , ("’", "'")
    , --
      ("—", "--")
    , --
      ("…", "...")
    ]

testPrintTags :: [EvernoteTag] -> IO ()
testPrintTags = traverse_ testPrintTag

testPrintTag :: EvernoteTag -> IO ()
testPrintTag (TagOpen s _) = T.putStrLn $ "<" <> s <> ">"
testPrintTag (TagClose s) = T.putStrLn $ "</" <> s <> ">"
testPrintTag (TagComment s) = T.putStrLn $ "comment:" <> s
testPrintTag (TagWarning s) = T.putStrLn $ "warning:" <> s
testPrintTag (TagPosition x y) = putStrLn $ show x <> "," <> show y
testPrintTag (TagText s) =
  if isNoteContentData s
    then testPrintTags (parseTags s)
    else T.putStrLn $ "text:" <> s
