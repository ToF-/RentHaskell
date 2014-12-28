import Test.Hspec
import Data.List (sort)
import Data.Map as Map (Map,insertWith,empty,(!),insert,elems,keys,findWithDefault,findMax)
import qualified Data.Map as Map 

testing = True 
-- True: pass tests. False: run program
-- passing an arg won't do because hspec wants to consume the arg too

main = if testing then tests else process

process :: IO ()
process = putStrLn "18"

os = [makeOrder 0 5 10
     ,makeOrder 3 7 14
     ,makeOrder 5 9 7
     ,makeOrder 6 9 8]        

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
        it "contains end time points" $ do
            Map.keys (plan os) `shouldBe` [3,5,6,10,14,15]

        it "maps time points to order ending at that point" $ do
            Map.toList (plan os) `shouldBe` 
                [(3,[Order {startTime = 0, endTime = 3, price = 0}])
                ,(5,[Order {startTime = 0, endTime = 5, price = 10}
                    ,Order {startTime = 3, endTime = 5, price = 0}])
                ,(6,[Order {startTime = 5, endTime = 6, price = 0}])
                ,(10,[Order {startTime = 3, endTime = 10, price = 14}])
                ,(14,[Order {startTime = 5, endTime = 14, price = 7}])
                ,(15,[Order {startTime = 6, endTime = 15, price = 8}])]

    describe "exploit" $ do
        it "map time to money" $ do
            Map.toList (exploit (plan os)) `shouldBe`
                [(3,0),(5,10),(6,10),(10,14),(14,17),(15,18)]
        
    describe "profit" $ do
        it "optimizes orders" $ do
            let os = [makeOrder 0 5 10
                     ,makeOrder 3 7 14
                     ,makeOrder 5 9 7
                     ,makeOrder 6 9 8]        
            profit os `shouldBe` 18
    

type Time  = Int
type Money = Int
data Order = Order { startTime :: Time, 
                     endTime   :: Time, 
                     price     :: Money }
    deriving (Eq, Ord, Show)
type Plan    = Map Time [Order] 
type Profits = Map Time Money

profit :: [Order] -> Money
profit = money . findMax . exploit . plan
    where money = snd

plan :: [Order] -> Plan
plan = foldr insertOrder empty . withNullOrders . sort 
    where
    insertOrder o pl = insertWith (++) (endTime o) [o] pl
    withNullOrders os = os ++ zipWith nullOrder times (tail times) 
        where 
        times = map startTime os
        nullOrder s s' = Order s s' 0

exploit :: Plan -> Profits
exploit pl = Map.map (maximum . map findProfit) pl
    where
    findProfit o   = findProfitAt (startTime o) + (price o)
    findProfitAt t = findWithDefault 0 t (exploit pl)

makeOrder :: Time -> Time -> Money -> Order
makeOrder s d p = Order s (s+d) p 

