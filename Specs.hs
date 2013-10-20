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
            forAll order $ \o -> optimizeL [o] == price o 
    
        it "when given 2 incompatible orders gives the bigger price" $ property $
            forAll incompatibleOrders $ \(o,p) -> optimizeL [o,p] == max (price o) (price p)   

        it "when given 2 compatible orders gives the sum of the prices" $ property $
            forAll compatibleOrders $ \(o,p) -> optimizeL [o,p] == price o + price p

        it "when given several orders gives the optimal solution" $ do
            optimizeL [(0,5,10),(3,7,9),(5,4,7),(6,5,8)] `shouldBe` 18

positiveInt :: Gen Int
positiveInt = do n <- arbitrary
                 return (1 + abs n) 

aTime :: Gen Int
aTime = choose (0,1000000)

aPrice :: Gen Int
aPrice = choose (0,100000)

order :: Gen Order
order = do startTime <- aTime
           duration  <- aTime
           price     <- aPrice
           return (startTime, duration, price) 
    
incompatibleOrders :: Gen (Order, Order)
incompatibleOrders = do startTime <- aTime
                        duration  <- aTime
                        price1    <- aTime
                        price2    <- aPrice
                        let order1    = (startTime,   duration+2, price1)
                            order2    = (startTime+1, duration+1, price2)
                        return (order1, order2)

compatibleOrders :: Gen (Order, Order)
compatibleOrders = do startTime <- aTime
                      duration  <- aTime
                      price1    <- aTime
                      price2    <- aPrice
                      let order1    = (startTime,   duration, price1)
                          order2    = (startTime+duration, duration, price2)
                      return (order1, order2)

mockOptimizer :: [Order] -> Int
mockOptimizer os = case length os of
                    4 -> 18
                    2 -> 8
                    1 -> 10  
