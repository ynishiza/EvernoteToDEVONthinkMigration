import Test.Hspec
-- import Text.Blaze.Html
-- import Text.Blaze.Html.Renderer.Pretty
import Utils
import Process
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Functor
import Data.Function
import Text.HTML.TagSoup
-- import TextShow

main :: IO ()
main = hspec $ describe "main" $ do
  it "test" $ do
    content <-
      T.readFile "./testdata/test.enex"
        <&> cleanseText
    let tags = parseTags content
    (result, _) <-
      traverseTags tags processTag
        & runProcess "test.log"
    let prettied = unescape $ renderTags result

    T.writeFile "./testdata/test_out.enex" prettied
    expected <- T.readFile "./testdata/test_expected.enex"

    prettied `shouldBe` expected
      where unescape text = text
                & T.replace "&lt;" "<"
                & T.replace "&gt;" ">"
                & T.replace ">" ">\n"
                & T.replace "&amp;quot;" "\""
                & T.replace "&quot;" "\""
