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
        outPath = dataDir </> "test_out.enex"
    content <- T.readFile dataPath
    (result, context) <-
      runProcess "test.log" content
        <&> first renderTags

    T.writeFile outPath result
    expected <- T.readFile expectedPath

    result `shouldBe` expected
    unescape result `shouldBe` unescape expected
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
                    , noteFontFaces = Set.fromList ["Andale Mono", "Monaco"]
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
