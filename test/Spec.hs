import Lib
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "Loading files" $ do
        it "bad filename" $ do
            someFunc `shouldThrow` anyException
