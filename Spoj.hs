-- be sure to have imported the content of Module Rent before running this program.
--
import Data.List (sort)
import Data.Map as Map (Map,insertWith,empty,(!),insert,elems,keys,findWithDefault,findMax)
import qualified Data.Map as Map 


os = [makeOrder 0 5 10
     ,makeOrder 3 7 14
     ,makeOrder 5 9 7
     ,makeOrder 6 9 8]        


type Time  = Int
type Money = Int
data Order = Order { startTime :: Time, 
                     endTime   :: Time, 
                     price     :: Money }
    deriving (Eq, Ord, Show)
type Plan    = Map Time [Order] 
type Profits = Map Time Money

process :: String -> String
process  = unlines . map show . solve . tail . map (map read) . map words . lines  

solve :: [[Int]] -> [Int]
solve [] = []
solve ([n]:xs) = solveProblem (take n xs) : solve (drop n xs)
    where
    solveProblem = profit . map (\[s,d,p] -> makeOrder s d p) 

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

main = interact process
