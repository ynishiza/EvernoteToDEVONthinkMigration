#!/usr/bin/env stack
-- stack script --resolver nightly-2022-11-05 --compiler ghc-9.2.5 --package tagsoup --package bytestring --package hxt --package text

{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as B
import Data.Foldable
import Data.List (isSubsequenceOf)
import Debug.Trace
import System.Environment
import Text.HTML.TagSoup
import Data.Maybe (catMaybes)
import Data.Text qualified as T
import Data.Text (Text)
import Data.Text.IO qualified as T
import Data.Text.Encoding qualified as T
import GHC.Exts (IsString)

main :: IO ()
main = do
  (fileName : _) <- getArgs
  content <- T.readFile fileName
  let c = T.replace "<div><div><br /></div></div>" "<div></div>" content
  -- let parsed = xread content
  -- let parsed = xreadDoc content
  -- let parsed = parseHtmlContent content
  -- print parsed
  -- testPrintTrees parsed
  let parsed = parseTags c
      -- out = mapTag <$> parsed
      out = visitTags parsed
  -- testPrintTags parsed
  testPrintTags out
  -- B.writeFile "result.enex" $ renderTags out
  T.writeFile "result.enex" $ T.replace newlinePlaceholder "\n" $  renderTags out

type MyTag = Tag Text

newlinePlaceholder :: IsString s => s
newlinePlaceholder = "______NEWLINE______"

isInnerDoc :: Text -> Bool
isInnerDoc = T.isPrefixOf "<!DOCTYPE" . T.strip

isCodeBlock :: Attribute Text -> Bool
isCodeBlock (name, value) = name == "style" && "codeblock" `isSubsequenceOf` T.unpack value

hasCodeBlock :: [Attribute Text] -> Bool
hasCodeBlock = any isCodeBlock

mapTag :: MyTag -> MyTag
mapTag t@(TagOpen s attr) =
  trace ("trace open:" <> T.unpack s) $
    if any isCodeBlock attr
      then TagOpen s [("style", "background: blue; display: block;")]
      else t
-- TagOpen "p" [("style", "background: blue; display: block;")] else t
mapTag t@(TagText s) =
  trace ("trace text:{" <> T.unpack (T.take 100 s) <> "}") $
    if isInnerDoc s
      then
        let parsed = parseTags s
         in trace "trace code:" $ TagText (renderTags (mapTag <$> parsed))
      else t
mapTag t = t

visitTags :: [MyTag] -> [MyTag]
visitTags [] = []
visitTags (t : rest)
  | TagOpen s attr <- t, hasCodeBlock attr = let (x, y) = trace ("map code") $ mapCode rest
                                                  in x ++ visitTags y
  | TagText s <- t, isInnerDoc s = TagText (renderTags $ visitTags $ parseTags s):rest
  | otherwise = trace ("visit:" <> show t) $ t : visitTags rest

matchTags :: Text -> Int -> ([MyTag], [MyTag]) -> ([MyTag], [MyTag])
matchTags tagName matchCount (matched, (t : rest))
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

mapCode :: [MyTag] -> ([MyTag], [MyTag])
mapCode tags = ((open : (catMaybes $ mapInner <$> inner)) ++ [close], rest)
  where
    (inner, rest) = matchTags "div" 0 ([], tags)
    newTag = "pre"
    -- open = TagOpen newTag [("style", "background: blue; display: block; width: 100%; white-space: pre-line")]
    -- open = TagOpen newTag []
    open = TagOpen newTag [("style", "background: blue; margin: 0; padding: 0; font-size: 12px;")]
    close = TagClose newTag

    -- Need p
    -- mapInner (TagOpen "div" _) = Just $ TagOpen "p" [("style", "background: silver; display: block; width: 100%; white-space: pre-line; margin: 0; padding: 0")]
    -- mapInner (TagClose "div") = Just $ TagClose "p" 
    -- mapInner (TagOpen "br/" _) = Nothing
    mapInner (TagOpen "div" _) = Just $ TagText ""
    mapInner (TagClose "div") = Just $ TagText newlinePlaceholder

    -- br may be 
    mapInner (TagOpen "br" _) = Just $ TagText ""
    mapInner (TagClose "br" ) = Just $ TagText ""
    mapInner (TagOpen _ _) = Just $ TagText ""
    mapInner (TagClose _) = Just $ TagText ""
    mapInner t = Just t

-- \| s == tagName, matchCount == 0 = matched
-- \| s == tagName = matchTags tagName (matchCount - 1) (matched ++ [t]) tags
-- \| otherwise = matchTags tagName matchCount (matched ++ [t]) tags
-- matchTags tagName matchCount matched (t@(TagOpen s _):tags)
-- \| s == tagName = matchTags tagName (matchCount + 1) (matched ++ [t]) tags
-- \| otherwise = matchTags tagName matchCount (matched ++ [t]) tags

testPrintTags :: [MyTag] -> IO ()
testPrintTags = traverse_ testPrintTag

-- testPrintTrees :: XmlTrees -> IO ()
-- testPrintTrees = traverse_ testPrintTree

-- testPrintTree :: XmlTree -> IO ()
-- testPrintTree (NTree a t) = do
--   testPrintNode a
--   testPrintTrees t

-- testPrintNode :: XNode -> IO ()
-- testPrintNode (XText s) = putStrLn $ "XText:" <> s
-- testPrintNode (XBlob s) = putStrLn "Blob:"
-- testPrintNode (XCharRef s) = putStrLn "Char:"
-- testPrintNode (XEntityRef s) = putStrLn "Entity:"
-- testPrintNode (XCmt s) = putStrLn "Cmt:"
-- testPrintNode (XCdata s) =
--   if "<!DOCTYPE" `isPrefixOf` s
--     then testPrintTrees (xread s)
--     else putStrLn "CData:"
-- testPrintNode (XPi s t) = putStrLn "Pi:"
-- testPrintNode (XTag s t) = putStrLn "Tag:"
-- testPrintNode (XDTD s t) = putStrLn "DTD:"
-- testPrintNode (XAttr s) = putStrLn "Attr:"
-- testPrintNode (XError s t) = putStrLn "Error:"

testPrintTag :: MyTag -> IO ()
testPrintTag (TagOpen s _) = T.putStrLn $ "<" <> s <> ">"
testPrintTag (TagClose s) = T.putStrLn $ "</" <> s <> ">"
testPrintTag (TagComment s) = T.putStrLn $ "comment:" <> s
testPrintTag (TagWarning s) = T.putStrLn $ "warning:" <> s
testPrintTag (TagPosition x y) = putStrLn $ show x <> "," <> show y
testPrintTag (TagText s) =
  if isInnerDoc s
    then testPrintTags (parseTags s)
    else T.putStrLn $ "text:" <> s
