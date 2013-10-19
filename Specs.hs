-- Spec.hs
import Test.Hspec
import Rent

main :: IO ()
main = hspec $ do
    describe "process" $ do
        it "scan input for a case with orders and output a solution" $ do
            process "1\r4\r0 5 10\r3 7 14\r5 9 7\r6 9 8" `shouldBe` "18"

        it "scan several cases and give several solutions" $ do
            process "2\r1\r0 5 10\r2\r5 9 7\r6 9 8" `shouldBe` "10\r8"
