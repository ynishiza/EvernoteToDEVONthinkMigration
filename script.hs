#!/usr/bin/env stack
{-
  stack script
    --resolver nightly-2022-11-05 --compiler ghc-9.2.5
    --package tagsoup
    --package bytestring --package text
    --package filepath
    --package mtl
    --package monad-logger
    --package text
    --package text-show
    --package containers
    --package directory
-}
{- FOURMOLU_DISABLE -}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# HLINT ignore "Use mapMaybe" #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{- FOURMOLU_ENABLE -}

import Utils
import Control.Monad.Logger
import Control.Monad.State
import Data.Foldable
import Data.Function ((&))
import Data.Functor
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.Directory
import System.Environment
import System.FilePath
import Text.HTML.TagSoup
import TextShow

type M m = (MonadLogger m, MonadState Context m, MonadIO m)

data Context = Context
  { codeBlocks :: Int
  , tableCodeBlocks :: Int
  , styles :: Set Text
  }
  deriving stock (Eq, Show)

initialContext :: Context
initialContext = Context 0 0 Set.empty

main :: IO ()
main = do
  (inputPath : _) <- getArgs
  content <- T.readFile inputPath

  --
  -- let c = T.replace "<div><div><br /></div></div>" "<div></div>" content
  let fileName = takeBaseName inputPath
      outPath = takeDirectory inputPath </> fileName <> "_out" <.> "enex"
      logPath = takeDirectory inputPath </> fileName <> "" <.> "log"
      tags = parseTags content

  removeFileIfNotExists logPath
  (out, _) <-
    traverseTags tags processTag
      & run logPath
  renderTags out
    -- & T.replace newlinePlaceholder "\n"
    & T.writeFile outPath

instance TextShow EvernoteTag where showb tag = showb (show tag)
instance TextShow Context where showb tag = showb (show tag)

removeFileIfNotExists :: FilePath -> IO ()
removeFileIfNotExists path = do
  exists <- doesFileExist path
  when exists $ removeFile path

-- instance TextShow  where showb tag = showb (show tag)

run :: FilePath -> LoggingT (StateT Context IO) a -> IO (a, Context)
run logPath m =
  runFileLoggingT logPath m'
    & flip runStateT initialContext
 where
  m' = do
    result <- m
    context <- get
    logInfoN $ "context:" <> showt context
    liftIO $ T.putStrLn $ "context:" <> showt context
    return result

traverseTags :: M m => [EvernoteTag] -> (EvernoteTag -> [EvernoteTag] -> m ([EvernoteTag], [EvernoteTag])) -> m [EvernoteTag]
traverseTags [] _ = return []
traverseTags (tag : rest) f
  | TagText text <- tag
  , isNoteContentData text = do
      inner <- TagText . renderTags <$> traverseTags (parseTags text) f
      (inner :) <$> traverseTags rest f
  | otherwise = do
      (processed, rest') <- f tag rest
      (processed ++) <$> traverseTags rest' f

processTag :: M m => EvernoteTag -> [EvernoteTag] -> m ([EvernoteTag], [EvernoteTag])
processTag tag rest
   -- case:
  | TagOpen name attr <- tag
  , codeBlockTag == name
  , hasCodeBlockAttribute attr = do
      logInfoN "visitTags: codeBlock"
      modify (\m@Context{codeBlocks} -> m{codeBlocks = codeBlocks + 1})
      return $ mapCodeBlockAsHighlight rest
   -- case:
  | isTagOpenFor "table" tag
  , isTableCodeBlock (tag : rest) = do
      logInfoN "visitTags: table"
      modify (\m@Context{tableCodeBlocks} -> m{tableCodeBlocks = tableCodeBlocks + 1})
      let (inner, rest') = matchTagsInit "table" rest
      return (createCodeBlockFromContent inner, rest')
   -- case:
  | isTagOpenFor "hr" tag = return ([], rest)
  | isTagCloseFor "hr" tag = return (horizontalLine, rest)
   -- case:
  | TagOpen "note" _ <- tag
  , (TagOpen "title" _ : TagText title : _) <- rest = do
      let message = "Note:" <> title
      liftIO $ T.putStrLn message
      logInfoN message
      return ([tag], rest)
   -- case:
  | TagOpen _ attr <- tag = do
      let elementStyles =
            filter ((== "style") . fst) attr
              <&> snd
              & filter ("font" `T.isInfixOf`)
              & Set.fromList
      modify (\m@Context{styles} -> m{styles = Set.union styles elementStyles})
      return ([tag], rest)
  | otherwise = do
      logInfoN ("visitTags other:" <> showt tag)
      return ([tag], rest)

-- visitTags :: [EvernoteTag] -> [EvernoteTag]
-- visitTags [] = []
-- visitTags tags@(tag : rest)
--   | TagOpen name attr <- tag,
--     codeBlockTag == name,
--     hasCodeBlockAttribute attr =
--       let (codeBlock, rest') = trace "visitTags: codeBlock" $ mapCodeBlockAsHighlight rest
--        in codeBlock ++ visitTags rest'
--   | isTagOpenFor "table" tag,
--     isTableCodeBlock tags =
--           let (inner, rest') = matchTagsInit "table" rest
--           in createCodeBlockFromContent inner ++ visitTags rest'
--   | isTagOpenFor "hr" tag = visitTags rest
--   | isTagCloseFor "hr" tag = horizontalLine ++ visitTags rest
--   | TagText text <- tag,
--     isNoteContentData text =
--       trace "visitTags: text" $ TagText (renderTags $ visitTags $ parseTags text) : visitTags rest
--   | otherwise =
--       trace ("visitTags:" <> show tag) $ tag : visitTags rest

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
