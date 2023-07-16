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
    --package time
    --package regex-tdfa
-}
{- FOURMOLU_DISABLE -}
{-# LANGUAGE OverloadedStrings #-}
{- FOURMOLU_ENABLE -}

import Control.Lens hiding (Context, (<.>))
import Control.Monad.State
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime
import Process
import System.Directory
import System.Environment
import System.FilePath
import Text.HTML.TagSoup

-- | main
main :: IO ()
main = do
  (inputPath : _) <- getArgs
  content <-
    T.readFile inputPath

  timezone <- getCurrentTimeZone
  timestamp <-
    getCurrentTime
      <&> utcToLocalTime timezone
      <&> formatTime defaultTimeLocale "%m%d_%H%M%S"

  let fileName = takeBaseName inputPath
      baseDir = takeDirectory inputPath
      outPath = baseDir </> fileName <> "_" <> timestamp <> "_out" <.> "enex"
      logPath = baseDir </> fileName <> "" <.> "log"
      infoPath = baseDir </> fileName <> "_info" <.> "json"

  (result, context) <- runProcess logPath content
  removeFileIfNotExists logPath
  let allNoteFonts =
        ( context
            & toListOf (_notes . traverse . _noteFonts)
            & foldr Set.union Set.empty
        )
          `Set.union` ( context
                          & toListOf (_notes . traverse . _noteFontFaces)
                          & foldr Set.union Set.empty
                      )
  renderTags result
    & T.writeFile outPath
  toContextText context
    & T.writeFile infoPath
  T.putStrLn $ "fonts:" <> T.intercalate "," (Set.toList allNoteFonts)

removeFileIfNotExists :: FilePath -> IO ()
removeFileIfNotExists path = do
  exists <- doesFileExist path
  when exists $ removeFile path
