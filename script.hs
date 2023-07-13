#!/usr/bin/env stack
{-
  stack script
    --resolver nightly-2022-11-05 --compiler ghc-9.2.5
    --package tagsoup
    --package bytestring --package text
    --package filepath
-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use mapMaybe" #-}

import Data.Foldable
import Data.Function ((&))
import Data.List (isSubsequenceOf)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Debug.Trace
import GHC.Exts (IsString)
import System.Environment
import System.FilePath
import Text.HTML.TagSoup
import Text.StringLike

main :: IO ()
main = do
  (inputPath : _) <- getArgs
  content <- T.readFile inputPath

  --
  -- let c = T.replace "<div><div><br /></div></div>" "<div></div>" content
  let fileName = takeBaseName inputPath
      outPath = takeDirectory inputPath </> fileName <> "_out" <.> "enex"
      tags = parseTags content
      out = visitTags tags
  renderTags out
    -- & T.replace newlinePlaceholder "\n"
    & T.writeFile outPath

-- testPrintTags out

type EvernoteTag = Tag Text

type EvernoteAttribute = Attribute Text

-- newlinePlaceholder :: IsString s => s
-- newlinePlaceholder = "______NEWLINE______"

-- Evernote note content is a CDATA of the form
--
--   <![CDATA[<!DOCTYPE en-note SYSTEM ..><en-note> ... </en-note>]]>
isNoteContentData :: Text -> Bool
isNoteContentData t = "<!DOCTYPE en-note" `T.isPrefixOf` t'
  where
    t' = T.strip t

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
isTagCloseFor s (TagClose t )  = t == s
isTagCloseFor _ _ = False

horizontalLine :: [EvernoteTag]
horizontalLine = [
  TagOpen "div" [("style", "text-align: center;")], 
  TagText "--------------------------------------------------------------------------------------------------------", 
  TagClose "div"]

isTableCodeBlock :: [EvernoteTag] -> Bool
isTableCodeBlock tags = inner
  & filter (isTagOpenFor "tr")
  & length
  & (== 1)
  where
    (inner, _) = matchTagsInit "table" tags

visitTags :: [EvernoteTag] -> [EvernoteTag]
visitTags [] = []
visitTags (tag : rest)
  | TagOpen name attr <- tag,
    codeBlockTag == name,
    hasCodeBlockAttribute attr =
      let (codeBlock, rest') = trace "visitTags: codeBlock" $ mapCodeBlockAsHighlight rest
       in codeBlock ++ visitTags rest'
  | isTagOpenFor "hr" tag = visitTags rest
  | isTagCloseFor "hr" tag = horizontalLine ++ visitTags rest
  | TagText text <- tag,
    isNoteContentData text =
      trace "visitTags: text" $ TagText (renderTags $ visitTags $ parseTags text) : visitTags rest
  | otherwise =
      trace ("visitTags:" <> show tag) $ tag : visitTags rest

matchTagsInit :: Text -> [EvernoteTag] -> ([EvernoteTag], [EvernoteTag])
matchTagsInit tagName tags = matchTags tagName 0 ([], tags)

matchTags :: Text -> Int -> ([EvernoteTag], [EvernoteTag]) -> ([EvernoteTag], [EvernoteTag])
matchTags tagName matchCount (matched, t : rest)
  | TagClose s <- t,
    s == tagName =
      if matchCount == 0
        then (matched, rest)
        else matchTags tagName (matchCount - 1) (matched', rest)
  | TagOpen s _ <- t, s == tagName = matchTags tagName (matchCount + 1) (matched', rest)
  | otherwise = matchTags tagName matchCount (matched', rest)
  where
    matched' = matched ++ [t]
matchTags tagName _ (matched, []) = error $ "Failed to find closing match: " <> T.unpack tagName <> "\n" <> show matched

mapCodeBlockAsHighlight :: [EvernoteTag] -> ([EvernoteTag], [EvernoteTag])
mapCodeBlockAsHighlight tags = (createCodeBlockFromContent content, rest)
  where
    -- (content, rest) = matchTags codeBlockTag 0 ([], tags)
    (content, rest) = matchTagsInit codeBlockTag tags

createCodeBlockFromContent :: [EvernoteTag] -> [EvernoteTag]
createCodeBlockFromContent content = (openTag : mapContent content) ++ [closeTag]
  where
    -- Note: map codeblock to <pre>
    --
    -- When the note is converted to rich text, we want to ensure that the entire code block is highlighted.
    -- Using a simple <div> block does not work because:
    --
    -- a) only text is highlighted, not whole line
    -- b) empty lines in the code block are not highlighted
    --
    -- Using <pre> seems to work
    -- a) entire line is highlighted in <pre>
    -- b) empty line is highlighted in <pre>
    --    However, empty lines must be a new line char "\n", not <br>
    newTag = "pre"
    openTag = TagOpen newTag [("style", codeBlockStyle)]
    closeTag = TagClose newTag

    mapContent [] = []
    -- Note: each line in a code block is represented by a div
    -- e.g.
    --    <div>x = 1</div>
    --    <div>y = 1</div>
    --    <div><div>z =1</div>          May also be nested sometimes, in which case the outer div doesn't count as a new line
    --         <div>a=10</div></div>
    --
    mapContent (TagOpen "div" _ : xs) = mapContent xs
    -- Note: treat as a single line if nested divs
    mapContent (TagClose "div" : TagClose "div" : TagClose "div" : xs) = TagText "\n" : mapContent xs
    mapContent (TagClose "div" : TagClose "div" : xs) = TagText "\n" : mapContent xs
    -- mapContent (TagClose "div" : xs) = TagText newlinePlaceholder : mapContent xs
    mapContent (TagClose "div" : xs) = TagText "\n" : mapContent xs
    -- br is a close tag <br />
    mapContent (TagClose "br" : xs) = mapContent xs
    -- Drop other styling tags in code blocks
    mapContent (TagOpen _ _ : xs) = mapContent xs
    mapContent (TagClose _ : xs) = mapContent xs
    mapContent (t : xs) = t : mapContent xs

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
