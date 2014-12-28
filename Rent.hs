import Test.Hspec
import Data.List (sort)
import Data.Map as Map (insertWith,empty,(!),insert,elems,keys,findWithDefault)
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

        it "maps time points to order ending at that point" $ do
            Map.toList (plan os) `shouldBe` 
                [(2,[Order {startTime = 0, endTime = 2, price = 0}])
                ,(3,[Order {startTime = 2, endTime = 3, price = 0}])
                ,(5,[Order {startTime = 0, endTime = 5, price = 10}])
                ,(8,[Order {startTime = 2, endTime = 8, price = 12}
                    ,Order {startTime = 3, endTime = 8, price = 10}])]
        
    describe "profit" $ do
        it "optimizes orders" $ do
            let os = [makeOrder 0 5 10
                     ,makeOrder 3 7 14
                     ,makeOrder 5 9 7
                     ,makeOrder 6 9 8]        
            profit os `shouldBe` 18
    

type Time = Int
type Money = Int
data Order = Order { startTime :: Time, endTime :: Time, price :: Money }
    deriving (Eq, Ord, Show)

makeOrder :: Time -> Time -> Money -> Order
makeOrder s d p = Order s (s+d) p 

type Plan = Map.Map Time [Order] 

plan :: [Order] -> Plan
plan = foldr insertOrder empty . withNullOrders . sort 
    where
    insertOrder o pl = insertWith (++) (endTime o) [o] pl
    withNullOrders os = os ++ zipWith nullOrder times (tail times) 
        where 
        times = map startTime os
        nullOrder s s' = Order s s' 0
 

type Profits = Map.Map Time Money

profit :: [Order] -> Money
profit os = last $ elems profits
    where
    profits  = foldl ins empty (keys p)
    p        = plan os
    ins t k  = insert k best t
        where
        best     = maximum (map profit (p!k)) 
        profit o = price o + (findWithDefault 0 (startTime o) t)
