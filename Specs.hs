-- Spec.hs
import Test.Hspec
import Test.QuickCheck
import Rent

main :: IO ()
main = hspec $ do
    describe "process" $ do
        it "scan input for a case with orders and output a solution" $ do
            process mockOptimizer "1\n4\n0 5 10\n3 7 14\n5 9 7\n6 9 8" `shouldBe` "18\n"

        it "scan several cases and give several solutions" $ do
            process mockOptimizer "2\n1\n0 5 10\n2\n5 9 7\n6 9 8" `shouldBe` "10\n8\n"

    describe "extract" $ do
        it "forms a list of lists of orders" $ do
            extract [[1],[0,5,10],[2],[3,7,9],[5,9,8]] `shouldBe` [[(0,5,10)],[(3,7,9),(5,9,8)]] 

    describe "optimize (list)" $ do
        it "when given 1 order gives the order price" $ property $
            \o -> optimizeL [o] `shouldBe` price o 
    
        it "when given 2 incompatible orders gives the bigger price" $ property $
            \(o,p) -> not (compatible o p) ==> optimizeL [o,p] == max (price o) (price p)   

compatible :: Order -> Order -> Bool
compatible (s,d,_) (s',d',_) = (s' >= s+d) || (s >= s'+d') 
mockOptimizer :: [Order] -> Int
mockOptimizer os = case length os of
                    4 -> 18
                    2 -> 8
                    1 -> 10  
