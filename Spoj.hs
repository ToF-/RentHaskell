-- concat the code below to Spoj.hs --

import qualified Data.Map as Map
import Data.Map (insertWith, findWithDefault, assocs,Map, empty, keys, insert, (!), findMax, findMin, toList, lookup)
import Data.List (sort)

type Order = (Time, Time, Money)
type Time  = Int
type Money = Int

order :: Time -> Time -> Money -> Order
order s d p = (s,d,p)

start :: Order -> Time
start (s,_,_) = s

end :: Order -> Time
end (s,d,_) = s + d

price :: Order -> Money
price (_,_,p) = p

type Bid  = (Money,Time)
type Plan = Map Time [Bid]

time :: Bid -> Time
time = snd

money :: Bid -> Money
money = fst 

plan :: [Order] -> Plan
plan os = foldr insertOrder initial os
    where
    insertOrder o = insertWith (++) (end o) [(price o, start o)]
    initial       = foldr insertOrderStart empty os
    insertOrderStart o = insertWith (++) (start o) []

minStartTime :: Plan -> Time
minStartTime = fst . findMin  


type Profits = (Map Time Money, Money)

profits :: Plan -> Profits
profits pl = foldl insertProfit initial (tail $ assocs pl)
    where
    initial :: Profits
    initial = (insert (minStartTime pl) 0 empty,0)

    insertProfit :: Profits -> (Time,[Bid]) -> Profits
    insertProfit (pr,m) (t,bs) = (insert t newValue pr, newValue)
        where
        newValue = max m $ maxValue bs 

        maxValue [] = 0
        maxValue xs = (maximum . map value) xs 

        value :: Bid -> Money
        value b = money b + findWithDefault 0 (time b) pr

profit :: [Order] -> Money
profit = snd . profits . plan

solutions :: [[Int]] -> [Int]
solutions = solutions' . tail 
    where
    solutions' :: [[Int]] -> [Int]
    solutions' [] = []
    solutions' ([n]:ns) = solve (take n ns) : solutions' (drop n ns) 
    
    solve :: [[Int]] -> Int
    solve = profit . map (\[s,d,p] -> order s d p)

process :: String -> String
process = unlines . map show . solutions . map (map read . words) . lines

main = interact process
