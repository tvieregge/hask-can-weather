import Lib
import qualified Data.Text.Lazy.IO as TLIO
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "Loading files" $ do
        it "reads files" $ do
            let bad = "./data/Xeng-daily-01011954-12311954.csv.1"
            let good = "./data/eng-daily-01011954-12311954.csv.1"
            -- someFunc [bad] `shouldReturn` ()
            -- text <- (TLIO.readFile "./data/eng-daily-01011954-12311954.csv.1")
            -- someFunc [good] `shouldReturn` ()
            someFunc [bad,good] `shouldReturn` ()
