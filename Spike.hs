-- Spike.hs
import Test.Hspec
import Data.Graph


main :: IO ()
main = hspec $ do
    describe "Spike -- experimenting with data structures\n" $ do
        it "does nothing yet"  $ do
            True `shouldBe` True    
