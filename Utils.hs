{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}

module Utils (
  isNoteContentData,
  codeBlockTag,
  isCodeBlockAttribute,
  hasCodeBlockAttribute,
  codeFontRegex,
  textFontRegex,
  codeBlockColor,
  codeBlockStyle,
  isTagOpenFor,
  isTagCloseFor,
  matchTagsInit,
  horizontalLine,
  isTableCodeBlock,
  EvernoteTag,
  EvernoteAttribute,
  testPrintTags,
  getFontFamily,
  dataLensRules,
  baseFontSize,
  codeStyle,
  codeFontFamily,
  styleAttribute,
  styleAttributes,
  cleanseText,
  isCodeFont,
  isTextFont,
  textFontFamily,
  existingCodeFonts,
  existingTextFonts,
  normalizeTextFont,
) where

import Control.Lens
import Data.Foldable
import Data.List (isSubsequenceOf)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TL
import GHC.Exts (IsString)
import Text.CSS.Parse
import Text.CSS.Render
import Text.HTML.TagSoup
import Text.Regex.TDFA hiding (matchCount)
import Text.StringLike

type EvernoteTag = Tag Text

type EvernoteAttribute = Attribute Text

{-

    <note><title>Cheatsheet: Markdown</title><content><![CDATA[<!DOCTYPE en-note SYSTEM "http://xml.evernote.com/pub/enml2.dtd"><en-note> ...

    <note><title>Cheatsheet [.NET]: cheatsheet</title><content><![CDATA[<?xml version="1.0" encoding="UTF-8" standalone="no"?><!DOCTYPE en-note SYSTEM "http://xml.evernote.com/pub/enml2.dtd">
-}
isNoteContentData :: Text -> Bool
isNoteContentData text = "<!DOCTYPE en-note" `T.isPrefixOf` t' || "<?xml version" `T.isPrefixOf` t'
 where
  t' = T.strip text

isCodeFont :: Text -> Bool
isCodeFont s = s =~ codeFontRegex

isTextFont :: Text -> Bool
isTextFont s = s =~ textFontRegex

existingCodeFonts :: [Text]
existingCodeFonts =
  [ "Andale Mono"
  , "Monaco"
  , "monospace"
  ]

codeFontRegex :: Text
codeFontRegex = T.intercalate "|" (f <> (T.toLower <$> f))
  where f = (\x -> "(" <> x <> ")") <$> existingCodeFonts

existingTextFonts :: [Text]
existingTextFonts =
  -- IMPORTANT: longer names should be first in order to make sure they get replaced first
  [ "Helvetica Neue"
  , "Helvetica"
  , "helvetica"
  , "Arial Black"
  , "Arial"
  , "arial"
  , "courier new"
  , "courier"
  , "UICTFontTextStyleBody"
  , "Times"
  , "Times New Roman"
  , "sans-serif"
  ]

textFontRegex :: Text
textFontRegex = T.intercalate "|" (f <> (T.toLower <$> f))
  where f = (\x -> "(" <> x <> ")") <$> existingTextFonts

normalizeTextFont :: Text -> Text
normalizeTextFont text = replaceTextWithRegex textFontRegex textFontFamily text

replaceTextWithRegex :: Text -> Text -> Text -> Text
replaceTextWithRegex regex replacement text = if T.null y then x <> z else x <> replacement <> z
 where
  (x, y, z) = text =~ regex

codeBlockTag :: IsString s => s
codeBlockTag = "div"

-- Evernote code block is of the form
--
--    <div style="... en-codeblock:true;">
isCodeBlockAttribute :: EvernoteAttribute -> Bool
isCodeBlockAttribute (name, value) = name == "style" && "en-codeblock" `isSubsequenceOf` T.unpack value

hasCodeBlockAttribute :: [EvernoteAttribute] -> Bool
hasCodeBlockAttribute = any isCodeBlockAttribute

codeBlockColor :: IsString s => s
codeBlockColor = "rgb(232, 232, 232)"

baseFontSize :: IsString s => s
baseFontSize = "14px"

styleAttribute :: (Text, Text) -> Text
styleAttribute v =
  renderAttr v
    & TL.toLazyText
    & TL.toStrict

styleAttributes :: [(Text, Text)] -> Text
styleAttributes v = T.intercalate ";" (styleAttribute <$> v) <> ";"

-- Note: slightly smaller than the base size
-- since monospace fonts appear larger.
codeFontSize :: IsString s => s
codeFontSize = "12px"

codeFontFamily :: IsString s => s
codeFontFamily = "Monaco, Menlo, Consolas, monospace"

textFontFamily :: IsString s => s
textFontFamily = "Helvetica Neue"

codeStyle :: Text
codeStyle =
  styleAttributes
    [ ("font-family", codeFontFamily)
    , ("font-size", codeFontSize)
    , ("color", "rgb(51, 51, 51)")
    ]

codeBlockStyle :: Text
codeBlockStyle =
  codeStyle
    <> styleAttributes
      [ ("background", codeBlockColor)
      , ("border-radius", "4px")
      , ("border", "1px solid rgba(0, 0, 0, 0.15)")
      ]

isTagOpenFor :: StringLike s => s -> Tag s -> Bool
isTagOpenFor s (TagOpen t _) = t == s
isTagOpenFor _ _ = False

isTagCloseFor :: StringLike s => s -> Tag s -> Bool
isTagCloseFor s (TagClose t) = t == s
isTagCloseFor _ _ = False

cleanseText :: Text -> Text
cleanseText text = foldr (uncurry T.replace) text toReplace
 where
  toReplace =
    [ -- Double quotes
      ("“", "\"")
    , ("”", "\"")
     -- Single quotes
      ,  ("‘", "'")
    , ("’", "'")
    , ("—", "--")
    ]

getFontFamily :: Text -> Maybe Text
getFontFamily text = case parseAttrs text of
  Left _ -> Nothing
  Right styles -> case find (isFontStyle . fst) styles of
    Nothing -> Nothing
    Just (_, font) -> Just font
 where
  isFontStyle x = x == "font-family" || x == "font"

horizontalLine :: [EvernoteTag]
horizontalLine =
  [ TagOpen "div" [("style", "text-align: center;")]
  , TagText "=============================================================="
  , TagClose "div"
  ]

isTableCodeBlock :: [EvernoteTag] -> Bool
isTableCodeBlock tags =
  inner
    & filter (isTagOpenFor "tr")
    & length
    & (== 1)
 where
  (inner, _) = matchTagsInit "table" (tail tags)

matchTagsInit :: Text -> [EvernoteTag] -> ([EvernoteTag], [EvernoteTag])
matchTagsInit tagName tags = matchTags tagName 0 ([], tags)

matchTags :: Text -> Int -> ([EvernoteTag], [EvernoteTag]) -> ([EvernoteTag], [EvernoteTag])
matchTags tagName matchCount (matched, t : rest)
  | TagClose s <- t
  , s == tagName =
      if matchCount == 0
        then (matched, rest)
        else matchTags tagName (matchCount - 1) (matched', rest)
  | TagOpen s _ <- t, s == tagName = matchTags tagName (matchCount + 1) (matched', rest)
  | otherwise = matchTags tagName matchCount (matched', rest)
 where
  matched' = matched ++ [t]
matchTags tagName _ (matched, []) = error $ "Failed to find closing match: " <> T.unpack tagName <> "\n" <> show matched

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

lensNamer :: FieldNamer
lensNamer = mappingNamer $ \s -> ['_' : s]

dataLensRules :: LensRules
dataLensRules =
  lensRules
    & set lensField lensNamer
