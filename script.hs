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
import Data.List (partition)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
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
  , noteFonts :: Set Text
  , noteFontFaces :: Set Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

$(makeLensesWith dataLensRules ''NoteInfo)
$(makeLensesWith dataLensRules ''Context)

isEqual :: Eq a => a -> a -> Bool
isEqual x y = x == y

emptyNote :: Text -> NoteInfo
emptyNote name = NoteInfo name 0 0 Set.empty Set.empty

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

{-
  Note format:
  In general, notes are of the form

    <note>
      <title>Modules: Aeson</title>
      <content>
        <![CDATA[<!DOCTYPE en-note SYSTEM "http://xml.evernote.com/pub/enml2.dtd">
          <en-note>

          </en-note>
        ]]>
      </content>
      <created>20230629T205234Z</created>
      <updated>20230701T021641Z</updated>
      <note-attributes>
        <author>Yui Nishizawa</author>
        <source>desktop.mac</source>
        <reminder-order>0</reminder-order>
      </note-attributes>
    </note>

  However, some notes have content beginning with `<?xml ...>`

  e.g.
    <note>
      <title>Cheatsheet [.NET]: cheatsheet</title>
      <content>
        <![CDATA[<?xml version="1.0" encoding="UTF-8" standalone="no"?><!DOCTYPE en-note SYSTEM "http://xml.evernote.com/pub/enml2.dtd">

-}
processTag :: M m => EvernoteTag -> [EvernoteTag] -> m ([EvernoteTag], [EvernoteTag])
processTag tag nextTags
  -- case: <note>
  -- Record beginning of new note
  | TagOpen "note" _ <- tag
  , (TagOpen "title" _ : TagText title : _) <- nextTags = do
      let message = "processTag: note - " <> title
      let note = emptyNote title
      _currentNote .= Just note
      _notes . at title ?= note
      liftIO $ T.putStrLn message
      logInfoN message
      return unchanged
  | TagClose "note" <- tag = do
      Just note <- use _currentNote
      _notes . at (noteName note) ?= note
      _currentNote .= Nothing
      return unchanged

  -- case: <en-note>
  --
  -- Set note-wide format
  -- IMPORTANT: don't set style on <en-note>
  -- i.e.
  --      <en-note style=...>           NO. Style doesn't get applied.
  --      <en-note><div style=...>      OK
  | TagOpen "en-note" enAttr <- tag = do
      logInfoN "processTag: en-note"
      return ([TagOpen "en-note" enAttr, TagOpen "div" [("style", styleAttribute ("font-size", baseFontSize))]], nextTags)
  | TagClose "en-note" <- tag =
      return ([TagClose "div", TagClose "en-note"], nextTags)
  -- case: code block
  -- Code block is a div of the form
  --
  --    <div style="... en-codeblock:true;">
  | TagOpen name attr <- tag
  , codeBlockTag == name
  , hasCodeBlockAttribute attr = do
      logInfoN "processTag: codeBlock"
      _currentNote . _Just . _noteCodeBlocks += 1
      return $ mapCodeBlockAsHighlight nextTags

  -- case: table code block
  -- Legacy code block is a table with 1 row and 1 column
  | isTagOpenFor "table" tag
  , isTableCodeBlock (tag : nextTags) = do
      logInfoN "processTag: table code"
      _currentNote . _Just . _noteTableCodeBlocks += 1
      let (inner, rest') = matchTagsInit "table" nextTags
      return (createCodeBlockFromContent inner, rest')

  -- case: horizontal line
  | isTagOpenFor "hr" tag = do
      logInfoN "processTag: hr"
      return ([], nextTags)
  | isTagCloseFor "hr" tag = return (horizontalLine, nextTags)
  -- case: <font face="Arial">
  --
  -- Change <font> tags to <span>s with style attributes since <font> is deprecated.
  | TagOpen "font" attr <- tag
  , Just (fontFace, _) <- findAttr "face" attr = do
      logInfoN ("processTag: font face=" <> fontFace)
      _currentNote . _Just . _noteFontFaces %= Set.union (Set.singleton fontFace)
      if isCodeFont fontFace
        then return ([TagOpen "span" [codeStyleAttr]], nextTags)
        else return ([TagOpen "span" [("style", styleAttribute ("font-family", fontFace))]], nextTags)
  -- case: there are some empty <font> tags for some reason.
  | TagOpen "font" _ <- tag = return ([TagOpen "span" []], nextTags)
  | TagClose "font" <- tag = return ([TagClose "span"], nextTags)
  -- case: other tags with style
  -- Unify styles
  | TagOpen name attr <- tag
  , Just (stylesText, attrRest) <- findAttr "style" attr
  , Just font <- getFontFamily stylesText = do
      logInfoN ("processTag: style element " <> showt tag)
      _currentNote . _Just . _noteFonts %= Set.union (Set.singleton font)
      return $ case () of
        ()
          | isCodeFont font -> ([TagOpen name (codeStyleAttr : attrRest)], nextTags)
          | otherwise -> unchanged

  -- case: rest
  | otherwise = do
      logInfoN ("processTag: other" <> showt tag)
      return unchanged
 where
  codeStyleAttr = ("style", codeStyle)
  unchanged = ([tag], nextTags)
  findAttr attrName attrs = case partition (isEqual attrName . fst) attrs of
    ([(_, content)], attrRest) -> Just (content, attrRest)
    _ -> Nothing

isCodeFont :: Text -> Bool
isCodeFont s = "Andale Mono" `T.isInfixOf` s || "Monaco" `T.isInfixOf` s

-- codeFont :: Text
-- codeFont =
--   renderAttrs
--     [ ("font-family", codeFontFamily)
--     , ("font-size", baseFontSize)
--     ]
--     & TL.toLazyText
--     & TL.toStrict

mapCodeBlockAsHighlight :: [EvernoteTag] -> ([EvernoteTag], [EvernoteTag])
mapCodeBlockAsHighlight tags = (createCodeBlockFromContent content, rest)
 where
  -- (content, rest) = matchTags codeBlockTag 0 ([], tags)
  (content, rest) = matchTagsInit codeBlockTag tags

createCodeBlockFromContent :: [EvernoteTag] -> [EvernoteTag]
createCodeBlockFromContent content = (openTag : mapContent content) ++ [closeTag]
 where
  -- Note: map Evernote codeblock to <pre>
  --
  -- When the note is converted to rich text, we want to ensure that the entire code block is highlighted.
  -- Using a simple <div> block does not work because:
  --
  -- a) only text is highlighted, not the whole line
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
  -- Note: each line in an Evernote codeblock is represented by a div
  -- e.g.
  --    <div style="box-sizing: border-box; padding: 8px; font-family: Monaco, Menlo, Consolas, &quot;Courier New&quot;, monospace; font-size: 12px; color: rgb(51, 51, 51); border-radius: 4px; background-color: rgb(251, 250, 248); border: 1px solid rgba(0, 0, 0, 0.15);-en-codeblock:true;">
    --    <div>x = 1</div>
    --    <div>y = 1</div>
    --    <div><div>z =1</div>          May also be nested sometimes, in which case the outer div doesn't count as a new line
    --         <div>a=10</div>
    --     </div>
    --   </div>
  --
  --
  mapContent (TagOpen "div" _ : xs) = mapContent xs
  -- Note: treat as a single line if nested divs
  mapContent (TagClose "div" : TagClose "div" : TagClose "div" : xs) = TagText "\n" : mapContent xs
  mapContent (TagClose "div" : TagClose "div" : xs) = TagText "\n" : mapContent xs
  mapContent (TagClose "div" : xs) = TagText "\n" : mapContent xs
  -- br is a close tag <br />
  mapContent (TagClose "br" : xs) = mapContent xs
  -- case: new line
  -- Some tags are on separate lines
  -- New line is meaningless except in <pre> elements
  -- e.g.
  --    <tr>
  --        content
  --    </tr>
  mapContent (TagText "\n" : xs) = mapContent xs
  -- Drop other styling tags in code blocks
  mapContent (TagOpen _ _ : xs) = mapContent xs
  mapContent (TagClose _ : xs) = mapContent xs
  mapContent (t : xs) = t : mapContent xs
