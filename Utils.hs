{-# LANGUAGE OverloadedStrings #-}

module Utils (
  isNoteContentData,
  codeBlockTag,
  isCodeBlockAttribute,
  hasCodeBlockAttribute,
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
) where

import Control.Lens
import Data.Foldable
import Data.List (isSubsequenceOf)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import GHC.Exts (IsString)
import Text.CSS.Parse
import Text.HTML.TagSoup
import Text.StringLike

type EvernoteTag = Tag Text

type EvernoteAttribute = Attribute Text

-- Evernote note content is a CDATA of the form
--
--   <![CDATA[<!DOCTYPE en-note SYSTEM ..><en-note> ... </en-note>]]>
isNoteContentData :: Text -> Bool
isNoteContentData text = "<!DOCTYPE en-note" `T.isPrefixOf` t'
 where
  t' = T.strip text

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

codeBlockStyle :: (IsString s, Semigroup s) => s
codeBlockStyle = "background: " <> codeBlockColor <> ";font-family: Monaco, Menlo, Consolas, &quot;Courier New&quot;, monospace; font-size: 12px; color: rgb(51, 51, 51); border-radius: 4px; border: 1px solid rgba(0, 0, 0, 0.15)"

isTagOpenFor :: StringLike s => s -> Tag s -> Bool
isTagOpenFor s (TagOpen t _) = t == s
isTagOpenFor _ _ = False

isTagCloseFor :: StringLike s => s -> Tag s -> Bool
isTagCloseFor s (TagClose t) = t == s
isTagCloseFor _ _ = False

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
  , TagText "--------------------------------------------------------------------------------------------------------"
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
