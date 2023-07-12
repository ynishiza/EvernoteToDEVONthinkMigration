#!/usr/bin/env stack
-- stack script --resolver nightly-2022-11-05 --compiler ghc-9.2.5 --package tagsoup --package bytestring --package hxt --package text

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use mapMaybe" #-}

import Data.Foldable
import Data.List (isSubsequenceOf)
import Debug.Trace
import System.Environment
import Text.HTML.TagSoup
import Data.Maybe (catMaybes)
import Data.Text qualified as T
import Data.Text (Text)
import Data.Text.IO qualified as T
import GHC.Exts (IsString)

main :: IO ()
main = do
  (fileName : _) <- getArgs
  content <- T.readFile fileName
  -- 
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

color :: IsString s => s
color = "rgb(232, 232, 232)"

newlinePlaceholder :: IsString s => s
newlinePlaceholder = "______NEWLINE______"

isInnerDoc :: Text -> Bool
isInnerDoc t = T.isPrefixOf "<!DOCTYPE" t' || T.isPrefixOf "<?xml version"  t'
  where t' = T.strip t

isCodeBlock :: Attribute Text -> Bool
isCodeBlock (name, value) = name == "style" && "codeblock" `isSubsequenceOf` T.unpack value

hasCodeBlock :: [Attribute Text] -> Bool
hasCodeBlock = any isCodeBlock

-- mapTag :: MyTag -> MyTag
-- mapTag t@(TagOpen s attr) =
--   trace ("trace open:" <> T.unpack s) $
--     if any isCodeBlock attr
--       then TagOpen s [("style", "background: blue; display: block;")]
--       else t
-- -- TagOpen "p" [("style", "background: blue; display: block;")] else t
-- mapTag t@(TagText s) =
--   trace ("trace text:{" <> T.unpack (T.take 100 s) <> "}") $
--     if isInnerDoc s
--       then
--         let parsed = parseTags s
--          in trace "trace code:" $ TagText (renderTags (mapTag <$> parsed))
--       else t
-- mapTag t = t

visitTags :: [MyTag] -> [MyTag]
visitTags [] = []
visitTags (t : rest)
  | TagOpen _ attr <- t, hasCodeBlock attr = let (x, y) = trace "visitTags: codeBlock" $ mapCode rest
                                                  in x ++ visitTags y
  | TagText s <- t, isInnerDoc s = trace "visitTags: text" $ TagText (renderTags $ visitTags $ parseTags s):visitTags rest
  | otherwise = trace ("visitTags:" <> show t) $ t : visitTags rest

matchTags :: Text -> Int -> ([MyTag], [MyTag]) -> ([MyTag], [MyTag])
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

mapCode :: [MyTag] -> ([MyTag], [MyTag])
mapCode tags = ((open : catMaybes (mapInner <$> inner)) ++ [close], rest)
  where
    (inner, rest) = matchTags "div" 0 ([], tags)
    newTag = "pre"
    -- open = TagOpen newTag [("style", "background: blue; display: block; width: 100%; white-space: pre-line")]
    -- open = TagOpen newTag []
    open = TagOpen newTag [("style", "background: " <> color <> ";font-family: Monaco, Menlo, Consolas, &quot;Courier New&quot;, monospace; font-size: 12px; color: rgb(51, 51, 51); border-radius: 4px; border: 1px solid rgba(0, 0, 0, 0.15)")]
    close = TagClose newTag

    -- Need p
    -- mapInner (TagOpen "div" _) = Just $ TagOpen "p" [("style", "background: silver; display: block; width: 100%; white-space: pre-line; margin: 0; padding: 0")]
    -- mapInner (TagClose "div") = Just $ TagClose "p" 
    -- mapInner (TagOpen "br/" _) = Nothing
    mapInner (TagOpen "div" _) = Just $ TagText ""
    mapInner (TagClose "div") = Just $ TagText newlinePlaceholder

    -- br may is a close tag <br /> 
    mapInner (TagClose "br" ) = Just $ TagText ""
    mapInner (TagOpen _ _) = Just $ TagText ""
    mapInner (TagClose _) = Just $ TagText ""
    mapInner t = Just t

testPrintTags :: [MyTag] -> IO ()
testPrintTags = traverse_ testPrintTag

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
--
-- \| s == tagName, matchCount == 0 = matched
-- \| s == tagName = matchTags tagName (matchCount - 1) (matched ++ [t]) tags
-- \| otherwise = matchTags tagName matchCount (matched ++ [t]) tags
-- matchTags tagName matchCount matched (t@(TagOpen s _):tags)
-- \| s == tagName = matchTags tagName (matchCount + 1) (matched ++ [t]) tags
-- \| otherwise = matchTags tagName matchCount (matched ++ [t]) tags

