{-# LANGUAGE OverloadedStrings #-}

module Utils (
  isTagOpenFor,
  isTagCloseFor,
  matchTags,
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
import Text.Regex.TDFA hiding (matchCount, before, after)
import Text.StringLike

type EvernoteTag = Tag Text

type EvernoteAttribute = Attribute Text

replaceTextWithRegex :: Text -> Text -> Text -> Text
replaceTextWithRegex regex replacement text = if T.null matched then before <> after else before <> replacement <> after
 where
  (before, matched, after) = text =~ regex

styleAttribute :: (Text, Text) -> Text
styleAttribute attr =
  renderAttr attr
    & TL.toLazyText
    & TL.toStrict

styleAttributes :: [(Text, Text)] -> Text
styleAttributes attrs = T.intercalate ";" (styleAttribute <$> attrs) <> ";"

isTagOpenFor :: StringLike s => s -> Tag s -> Bool
isTagOpenFor name (TagOpen t _) = t == name
isTagOpenFor _ _ = False

isTagCloseFor :: StringLike s => s -> Tag s -> Bool
isTagCloseFor name (TagClose t) = t == name
isTagCloseFor _ _ = False

getFontFamily :: Text -> Maybe Text
getFontFamily text = case parseAttrs text of
  Left _ -> Nothing
  Right styles -> case find (isFontStyle . fst) styles of
    Nothing -> Nothing
    Just (_, font) -> Just font
 where
  isFontStyle x = x == "font-family" || x == "font"

matchTags :: Text -> [EvernoteTag] -> ([EvernoteTag], [EvernoteTag])
matchTags name tags = _matchTags name 0 ([], tags)

_matchTags :: Text -> Int -> ([EvernoteTag], [EvernoteTag]) -> ([EvernoteTag], [EvernoteTag])
_matchTags name matchCount (matched, t : rest)
  | TagClose s <- t
  , s == name =
      if matchCount == 0
        then (matched, rest)
        else _matchTags name (matchCount - 1) (matched', rest)
  | TagOpen s _ <- t, s == name = _matchTags name (matchCount + 1) (matched', rest)
  | otherwise = _matchTags name matchCount (matched', rest)
 where
  matched' = matched ++ [t]
_matchTags name _ (matched, []) = error $ "Failed to find closing match: " <> T.unpack name <> "\n" <> show matched

lensNamer :: FieldNamer
lensNamer = mappingNamer $ \s -> ['_' : s]

dataLensRules :: LensRules
dataLensRules =
  lensRules
    & set lensField lensNamer
