{-# LANGUAGE OverloadedStrings #-}

module Utils (
  isTagOpenFor,
  isTagCloseFor,
  matchTagsInit,
  EvernoteTag,
  EvernoteAttribute,
  getFontFamily,
  dataLensRules,
  styleAttribute,
  styleAttributes,
  replaceTextWithRegex,
  (&),
  (<&>),
) where

import Control.Lens
import Data.Foldable
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TL
import Text.CSS.Parse
import Text.CSS.Render
import Text.HTML.TagSoup
import Text.Regex.TDFA hiding (matchCount)
import Text.StringLike

type EvernoteTag = Tag Text

type EvernoteAttribute = Attribute Text

replaceTextWithRegex :: Text -> Text -> Text -> Text
replaceTextWithRegex regex replacement text = if T.null y then x <> z else x <> replacement <> z
 where
  (x, y, z) = text =~ regex

styleAttribute :: (Text, Text) -> Text
styleAttribute v =
  renderAttr v
    & TL.toLazyText
    & TL.toStrict

styleAttributes :: [(Text, Text)] -> Text
styleAttributes v = T.intercalate ";" (styleAttribute <$> v) <> ";"

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

lensNamer :: FieldNamer
lensNamer = mappingNamer $ \s -> ['_' : s]

dataLensRules :: LensRules
dataLensRules =
  lensRules
    & set lensField lensNamer
