module Rent
where
-- concat the code below to Spoj.hs --

import qualified Data.Map as Map
import Data.Map (insertWith, findWithDefault, assocs,Map, empty, keys, insert, (!), findMax, findMin, toList, lookup, fromListWith,mapAccumWithKey)
import Data.List (sort)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)

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
        newValue = maximum $ m : map value bs

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

process :: ByteString -> ByteString
process = output . solutions . input
    where
    input :: ByteString -> [[Int]]
    input = (map $ map (fst . fromJust . BS.readInt)).(map BS.words).BS.lines

    output :: [Int] -> ByteString
    output = BS.unlines . map (BS.pack . show)

type Flight = (Money,Time)
type Position = (Money,[Flight])
type Plan' = Map Time Position

plan' :: [Order] -> Plan'
plan' = fromListWith mergeFlightLists . foldl positions []
    where
    mergeFlightLists :: Position -> Position -> Position 
    mergeFlightLists (_,fl) (_,fl') = (0,fl ++ fl')

    positions :: [(Time,Position)] -> Order -> [(Time,Position)]
    positions l o = start o : arrival o : l  

    arrival :: Order -> (Time,Position)
    arrival (s,d,p) = (s+d,(0,[(p,s)]))
 
    start :: Order -> (Time,Position)
    start (s,_,p) = (s,(0,[]))

profit' :: Plan' -> Money
profit' = fst . evaluate  
    where 
    evaluate :: Plan' -> (Money,Plan')
    evaluate = foldl profit (0,empty) . assocs 

    profit :: (Money,Plan') -> (Time,Position) -> (Money,Plan')
    profit (m,p) (t,(_,fl)) = (v,insert t (v,fl) p)
        where 
        v = maximum $ m:map profitFromFlight fl  

        profitFromFlight :: Flight -> Money
        profitFromFlight (m,s)  = (fst $ p!t) + m 
