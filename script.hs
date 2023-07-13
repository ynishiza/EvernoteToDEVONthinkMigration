#!/usr/bin/env stack
{-
  stack script
    --resolver nightly-2022-11-05 --compiler ghc-9.2.5
    --package tagsoup
    --package bytestring
    --package text
    --package filepath
    --package mtl
    --package monad-logger
    --package text
    --package text-show
    --package containers
    --package directory
    --package css-text
    --package aeson
    --package aeson-pretty
    --package lens
-}
{- FOURMOLU_DISABLE -}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# HLINT ignore "Use mapMaybe" #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{- FOURMOLU_ENABLE -}

import Control.Lens hiding (Context, (<.>))
import Control.Monad.Logger
import Control.Monad.State
import Data.Aeson hiding ((.=))
import Data.Aeson.Encode.Pretty
import Data.List (find)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text.IO qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import GHC.Generics (Generic)
import System.Directory
import System.Environment
import System.FilePath
import Text.HTML.TagSoup
import TextShow
import Utils

type M m = (MonadFail m, MonadLogger m, MonadState Context m, MonadIO m)

data Context = Context
  { currentNote :: Maybe NoteInfo
  , notes :: Map Text NoteInfo
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

data NoteInfo = NoteInfo
  { noteName :: Text
  , noteCodeBlocks :: Int
  , noteTableCodeBlocks :: Int
  , noteStyles :: Set Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

$(makeLensesWith dataLensRules ''NoteInfo)
$(makeLensesWith dataLensRules ''Context)

emptyNote :: Text -> NoteInfo
emptyNote name = NoteInfo name 0 0 Set.empty

initialContext :: Context
initialContext = Context Nothing Map.empty

main :: IO ()
main = do
  (inputPath : _) <- getArgs
  content <- T.readFile inputPath

  let fileName = takeBaseName inputPath
      baseDir = takeDirectory inputPath
      outPath = baseDir </> fileName <> "_out" <.> "enex"
      logPath = baseDir </> fileName <> "" <.> "log"
      infoPath = baseDir </> fileName <> "_info" <.> "json"
      tags = parseTags content

  removeFileIfNotExists logPath
  (result, context) <-
    traverseTags tags processTag
      & runProcess logPath
  renderTags result
    & T.writeFile outPath
  toContextText context
    & T.writeFile infoPath

instance TextShow EvernoteTag where showb tag = showb (show tag)
instance TextShow Context where showb tag = showb (show tag)

removeFileIfNotExists :: FilePath -> IO ()
removeFileIfNotExists path = do
  exists <- doesFileExist path
  when exists $ removeFile path

runProcess :: FilePath -> LoggingT (StateT Context IO) a -> IO (a, Context)
runProcess logPath m =
  runFileLoggingT logPath m'
    & flip runStateT initialContext
 where
  m' = do
    result <- m
    context <- get
    let context' = toContextText context
    logInfoN $ "context:" <> context'
    liftIO $ T.putStrLn $ "context:" <> context'
    return result

toContextText :: ToJSON a => a -> Text
toContextText context =
  toJSON context
    & encodePretty
    & TL.decodeUtf8
    & TL.toStrict

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
      _currentNote . _Just . _noteCodeBlocks += 1
      return $ mapCodeBlockAsHighlight rest
  -- case:
  | isTagOpenFor "table" tag
  , isTableCodeBlock (tag : rest) = do
      logInfoN "visitTags: table"
      _currentNote . _Just . _noteTableCodeBlocks += 1
      let (inner, rest') = matchTagsInit "table" rest
      return (createCodeBlockFromContent inner, rest')
  -- case:
  | isTagOpenFor "hr" tag = return ([], rest)
  | isTagCloseFor "hr" tag = return (horizontalLine, rest)
  -- case:
  | TagOpen "note" _ <- tag
  , (TagOpen "title" _ : TagText title : _) <- rest = do
      let message = "Note:" <> title
      let note = emptyNote title
      _currentNote .= Just note
      _notes . at title ?= note
      liftIO $ T.putStrLn message
      logInfoN message
      return ([tag], rest)
  | TagClose "note" <- tag = do
      Just note <- use _currentNote
      _notes . at (noteName note) ?= note
      _currentNote .= Nothing
      return ([tag], rest)
  -- case:
  | TagOpen _ attr <- tag
  , Just (_, stylesText) <- find ((== "style") . fst) attr
  , Just font <- getFontFamily stylesText = do
      _currentNote . _Just . _noteStyles %= Set.union (Set.singleton font)
      return ([tag], rest)
  -- case:
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
  mapContent (TagClose "div" : xs) = TagText "\n" : mapContent xs
  -- br is a close tag <br />
  mapContent (TagClose "br" : xs) = mapContent xs
  -- Drop other styling tags in code blocks
  mapContent (TagOpen _ _ : xs) = mapContent xs
  mapContent (TagClose _ : xs) = mapContent xs
  mapContent (t : xs) = t : mapContent xs
