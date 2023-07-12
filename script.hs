#!/usr/bin/env stack
-- stack script --resolver nightly-2022-11-05 --compiler ghc-9.2.5 --package tagsoup --package bytestring --package hxt

{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as B
import Data.Foldable
import Data.List (isPrefixOf, isSubsequenceOf)
import Data.Tree.NTree.TypeDefs
-- import Text.XML.HXT.DOM.TypeDefs
-- import Text.XML.HXT.Parser.XmlParsec
-- import Text.XML.HXT.Parser.HtmlParsec
import Debug.Trace
import System.Environment
import Text.HTML.TagSoup

main :: IO ()
main = do
  (fileName : _) <- getArgs
  content <- B.readFile fileName
  -- let parsed = xread content
  -- let parsed = xreadDoc content
  -- let parsed = parseHtmlContent content
  -- print parsed
  -- testPrintTrees parsed
  let parsed = parseTags content
      -- out = mapTag <$> parsed
      out = visitTags parsed
  -- testPrintTags parsed
  testPrintTags out
  B.writeFile "result.enex" $ renderTags out

type MyTag = Tag ByteString

isInnerDoc :: ByteString -> Bool
isInnerDoc = B.isPrefixOf "<!DOCTYPE" . B.strip

mapTag :: MyTag -> MyTag
mapTag t@(TagOpen s attr) =
  trace ("trace open:" <> B.unpack s) $
    if any isCodeBlock attr
      then TagOpen s [("style", "background: blue; display: block;")]
      else t
-- TagOpen "p" [("style", "background: blue; display: block;")] else t
mapTag t@(TagText s) =
  trace ("trace text:{" <> B.unpack (B.take 100 s) <> "}") $
    if isInnerDoc s
      then
        let parsed = parseTags s
         in trace "trace code:" $ TagText (renderTags (mapTag <$> parsed))
      else t
mapTag t = t

isCodeBlock :: Attribute ByteString -> Bool
isCodeBlock (name, value) = name == "style" && "codeblock" `isSubsequenceOf` B.unpack value

visitTags :: [MyTag] -> [MyTag]
visitTags [] = []
visitTags (t : rest)
  | TagText s <- t, isInnerDoc s = let 
                                  in TagText (renderTags $ visitTags $ parseTags s):rest
  | otherwise = trace ("visit:" <> show t) $ (t : visitTags rest)

matchTags :: ByteString -> Int -> ([MyTag], [MyTag]) -> ([MyTag], [MyTag])
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
matchTags tagName _ (matched, []) = error $ "Failed to find closing match: " <> B.unpack tagName <> "\n" <> show matched

mapCode :: [MyTag] -> ([MyTag], [MyTag])
mapCode tags = ((open : inner) ++ [close], rest)
  where
    (inner, rest) = matchTags "div" 0 ([], tags)
    open = TagOpen "p" [("style", "background: blue; display: block;")]
    close = TagClose "p"

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

testPrintTag :: Tag ByteString -> IO ()
testPrintTag (TagOpen s _) = B.putStrLn $ "<" <> s <> ">"
testPrintTag (TagClose s) = B.putStrLn $ "</" <> s <> ">"
testPrintTag (TagComment s) = B.putStrLn $ "comment:" <> s
testPrintTag (TagWarning s) = B.putStrLn $ "warning:" <> s
testPrintTag (TagPosition x y) = putStrLn $ show x <> "," <> show y
testPrintTag (TagText s) =
  if isInnerDoc s
    then testPrintTags (parseTags s)
    else B.putStrLn $ "text:" <> s
