import Control.Arrow (first)
import Data.Function
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Process
import System.FilePath
import Test.Hspec hiding (context)
import Text.HTML.TagSoup

dataDir :: FilePath
dataDir = "./test/data"

main :: IO ()
main = hspec $ describe "Test" $ do
  it "runProcess" $ do
    let dataPath = dataDir </> "test.enex"
        expectedPath = dataDir </> "test_expected.enex"
        expectedUnescapedPath = dataDir </> "test_expected_unescaped.enex"
        outPath = dataDir </> "test_out.enex"
        outUnescapedPath = dataDir </> "test_out_unescaped.enex"

    content <- T.readFile dataPath
    (result, context) <-
      runProcess "test.log" content
        <&> first renderTags

    -- Note: test with 2 format
    --  a) raw format
    --  b) unescaped format
    --
    -- The content is the same but the unescaped format is more human-friendly for debugging the test
    T.writeFile outPath result
    T.writeFile outUnescapedPath $ unescape result
    expectedResult <- T.strip <$> T.readFile expectedPath
    expectedUnescaped <- T.strip <$> T.readFile expectedUnescapedPath

    T.length (T.strip $ unescape result) `shouldBe` T.length expectedUnescaped
    T.strip (unescape result) `shouldBe` expectedUnescaped
    T.strip result `shouldBe` expectedResult
    context
      `shouldBe` Context
        { currentNote = Nothing
        , notes =
            Map.fromList
              [
                ( "Example typical note"
                , NoteInfo
                    { noteName = "Example typical note"
                    , noteCodeBlocks = 1
                    , noteTableCodeBlocks = 5
                    , noteFonts =
                        Set.fromList
                          [ "Andale Mono"
                          , "Arial"
                          , "Arial Black"
                          , "Courier"
                          , "Courier New"
                          , "Helvetica"
                          , "Helvetica Neue"
                          , "Monaco"
                          , "Times New Roman"
                          , "Verdana"
                          , "monospace"
                          , "sans-serif"
                          ]
                    , noteFontFaces = Set.fromList ["Andale Mono", "Arial", "Monaco"]
                    }
                )
              ,
                ( "Example: Note beginning with ?xml tag"
                , NoteInfo
                    { noteName = "Example: Note beginning with ?xml tag"
                    , noteCodeBlocks = 0
                    , noteTableCodeBlocks = 0
                    , noteFonts = Set.fromList []
                    , noteFontFaces = Set.fromList []
                    }
                )
              ]
        }
 where
  unescape text =
    text
      & T.replace "&lt;" "<"
      & T.replace "&amp;gt;" ">"
      & T.replace "&gt;" ">"
      & T.replace "&amp;quot;" "\""
      & T.replace "&quot;" "\""
      & T.replace "&amp;#39;" "\'"
      & T.replace "&#39;" "\'"
      -- separate tags on new line for readability
      & T.replace "div>" "div>\n"
      & T.replace "pre>" "pre>\n"
      & T.replace "en-note>" "en-note>\n"
      & T.replace "content>" "content>\n"
