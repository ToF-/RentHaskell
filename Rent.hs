import Test.Hspec
import Data.List (sort)
import Data.Map as Map (insertWith,empty)
import qualified Data.Map as Map 

testing = True 
-- True: pass tests. False: run program
-- passing an arg won't do because hspec wants to consume the arg too

main = if testing then tests else process

process :: IO ()
process = putStrLn "18"

tests :: IO ()
tests = hspec $ do
    describe "an order" $ do
        it "should have a start time" $ do
            startTime (makeOrder 3 5 10) `shouldBe` 3
        it "should have an end time" $ do
            endTime (makeOrder 3 5 10) `shouldBe` 3+5
        it "should have a price" $ do
            price (makeOrder 3 5 10) `shouldBe` 10

    describe "a plan" $ do
        let os = [makeOrder 3 5 10
                 ,makeOrder 2 6 12
                 ,makeOrder 0 5 10]
        it "contains end time points" $ do
            Map.keys (plan os) `shouldContain` [5,8]

        it "contains start time points" $ do
            Map.keys (plan os) `shouldContain` [0,2,3]

        it "maps time points to order ending at that point" $ do
            Map.toList (plan os) `shouldBe` 
                [(0,[])
                ,(2,[Order {startTime = 0, endTime = 2, price = 0}])
                ,(3,[Order {startTime = 2, endTime = 3, price = 0}])
                ,(5,[Order {startTime = 0, endTime = 5, price = 10}])
                ,(8,[Order {startTime = 3, endTime = 8, price = 10}
                    ,Order {startTime = 2, endTime = 8, price = 12}])]
            

type Time = Int
type Money = Int
data Order = Order { startTime :: Time, endTime :: Time, price :: Money }
    deriving (Eq, Ord, Show)

makeOrder :: Time -> Time -> Money -> Order
makeOrder s d p = Order s (s+d) p 

type Plan = Map.Map Time [Order] 


plan :: [Order] -> Plan
plan os = Map.insert firstTime [] $ foldr insertOrder Map.empty $ os ++ nullOrders
    where
    firstTime = startTime (head sortedOrders) 
    sortedOrders = sort os
    nullOrders = [Order (startTime o) (startTime o') 0 | (o,o') <- zip sortedOrders (tail sortedOrders)]  
    insertOrder o p = Map.insertWith (++) (endTime o) [o] p
            

