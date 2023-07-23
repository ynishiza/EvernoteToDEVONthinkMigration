{-# LANGUAGE OverloadedStrings #-}

module CodeContent (
  isCodeBlockAttribute,
  codeBlockColor,
  codeStyle,
  hasCodeBlockAttribute,
  codeBlockStyle,
  existingCodeFonts,
  codeFontRegex,
  codeBlockTag,
  isCodeFont,
  isTableCodeBlock,
) where

import Data.List (isSubsequenceOf)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Exts (IsString)
import Text.Regex.TDFA hiding (matchCount)
import Utils

-- Evernote code block is of the form
--
--    <div style="... en-codeblock:true;">
isCodeBlockAttribute :: EvernoteAttribute -> Bool
isCodeBlockAttribute (name, value) = name == "style" && "en-codeblock" `isSubsequenceOf` T.unpack value

hasCodeBlockAttribute :: [EvernoteAttribute] -> Bool
hasCodeBlockAttribute = any isCodeBlockAttribute

codeBlockColor :: IsString s => s
codeBlockColor = "rgb(232, 232, 232)"

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

codeFontSize :: IsString s => s
codeFontSize = "14px"

codeFontFamily :: IsString s => s
codeFontFamily = "Monaco, Menlo, Consolas, monospace"

existingCodeFonts :: [Text]
existingCodeFonts =
  [ "Andale Mono"
  , "Monaco"
  , "monospace"
  ]

codeFontRegex :: Text
codeFontRegex = T.intercalate "|" (opts <> (T.toLower <$> opts))
 where
  opts = (\x -> "(" <> x <> ")") <$> existingCodeFonts

codeBlockTag :: IsString s => s
codeBlockTag = "div"

isCodeFont :: Text -> Bool
isCodeFont text = text =~ codeFontRegex

isTableCodeBlock :: [EvernoteTag] -> Bool
isTableCodeBlock tags =
  inner
    & filter (isTagOpenFor "tr")
    & length
    & (== 1)
 where
  (inner, _) = matchTags "table" (tail tags)
