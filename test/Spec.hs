import Data.List
import qualified Data.Text.Lazy.IO as TLIO
import Lib
import Test.Hspec

main :: IO ()
main =
    hspec $ do
        describe "Loading files" $ do
            it "reads files" $
            -- someFunc [bad] `shouldReturn` ()
            -- text <- (TLIO.readFile "./data/eng-daily-01011954-12311954.csv.1")
            -- someFunc [good] `shouldReturn` ()
             do
                let ints = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
                    floats = [0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5]
                    testData =
                        map (\(a, b, c, d) -> DataItem a b c d) $
                        zip4 ints ints ints floats
                dataPipeline 5 testData `shouldBe`
                    DisplayItem
                        { displayYear = [1, 2, 3, 4, 5, 6]
                        , displayValue = [2.5, 3.5, 4.5, 5.5, 6.5, 7.5]
                        }
