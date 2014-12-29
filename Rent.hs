module Rent
where
-- concat the code below to Spoj.hs --

import Data.Map (insertWith, Map, empty)
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
        | x == y    = x:uniq xs
        | otherwise = x:uniq (y:xs)

