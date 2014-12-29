module Rent
where
-- concat the code below to Spoj.hs --
import Data.List (sort,nub)
import Data.Map as Map (Map,insertWith,empty,(!),insert,elems,keys,findWithDefault,findMax)
import qualified Data.Map as Map 

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
plan os = foldl insertOrder plan' nullOrders 
    where
    plan'      = foldl insertOrder empty os
    nullOrders = zipWith nullOrder times (tail times) 
    times      = nub $ sort $ concatMap (\o -> [startTime o, endTime o]) os  
    nullOrder s s'   = Order s s' 0
    insertOrder pl o = insertWith (++) (endTime o) [o] pl

exploit :: Plan -> Profits
exploit pl = Map.map (maximum . map findProfit) pl
    where
    findProfit o   = findProfitAt (startTime o) + (price o)
    findProfitAt t = findWithDefault 0 t (exploit pl)

makeOrder :: Time -> Time -> Money -> Order
makeOrder s d p = Order s (s+d) p 

