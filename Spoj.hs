-- concat the code below to Spoj.hs --

import qualified Data.Map as Map
import Data.Map (insertWith, Map, empty, keys, insert, (!), findMax, findMin, toList, lookup)
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

type Plan = Map Time [(Money,Time)]

time :: (Money,Time) -> Time
time = snd

money :: (Money, Time) -> Money
money = fst 

plan :: [Order] -> Plan
plan os = foldr insertOrder initial os
    where
    insertOrder o = insertWith (++) (end o) [(price o, start o)]
    initial       = foldr insertOrder empty (nullOrders os)
   
nullOrders :: [Order] -> [Order]
nullOrders os = zipWith (\t t' -> order t (t'-t) 0) ts (tail ts) 
    where ts = times os

times :: [Order] -> [Time]
times os = uniq $ sort $ (map start os) ++ (map end os) 
    where
    uniq []  = []
    uniq [x] = [x]
    uniq (x:y:xs)
        | x == y    = uniq (y:xs)
        | otherwise = x:uniq (y:xs)

minStartTime :: Plan -> Time
minStartTime = minimum . (map time) . snd . findMin  


type Profits = Map Time Money

profits :: Plan -> (Profits, Money)
profits pl = foldl insertProfit initial (toList pl)
    where
    initial :: (Profits, Money)
    initial = (insert (minStartTime pl) 0 empty,0)

    insertProfit :: (Profits,Money) -> (Time,[(Money,Time)]) -> (Profits,Money)
    insertProfit (pr,m) (t,vs) = (insert t (maxValue vs) pr, max m (maxValue vs))
        where
        maxValue :: [(Money,Time)] -> Money
        maxValue = maximum . map value

        value :: (Money,Time) -> Money
        value (m,t) = m + pr!t

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
