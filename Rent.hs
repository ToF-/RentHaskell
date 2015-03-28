module Rent
where
import qualified Data.Map as Map
import Data.Map (insertWith, findWithDefault, assocs,Map, empty, keys, insert, (!), findMax, findMin, toList, lookup, fromListWith, fromList, mapAccumWithKey)
import Data.List (sort)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)

type Order  = (Time, Time, Money)
type Time   = Int
type Money  = Int
type Bid    = (Money,Time)
type Plan   = Map Time [Bid]
type Profit = Map Time Money

profit' :: [Order] -> Money
profit' = fst . plan'

plan' :: [Order] -> (Money,Profit)
plan' os = foldl calc (0, Map.empty) $ schedule os
    where
    calc :: (Money, Profit) -> (Time, Maybe (Time, Money)) -> (Money, Profit)
    calc (m,pr) (t, Just (e,p)) = (m, insertWith max e (m+p) pr)
    calc (m,pr) (e, Nothing) = (max m (findWithDefault 0 e pr),pr)

schedule :: [Order] -> [(Time, Maybe (Time, Money))]
schedule os = sort $ (map bid os) ++ (map slot os)
    where
    bid :: Order -> (Time, Maybe (Time, Money))
    bid (start,duration,price) = (start, Just (start+duration,price))

    slot :: Order -> (Time, Maybe (Time, Money))
    slot (start,duration,_) = (start+duration, Nothing)

plan :: [Order] -> Plan
plan = fromListWith (++) . foldl positions []
    where
    positions :: [(Time,[Bid])] -> Order -> [(Time,[Bid])]
    positions orders (start,duration,price) = (start,[]) : (start+duration, [(price,start)]) : orders

profit :: [Order]-> Money
profit os = fst $ foldl calc (0, insert (fst $ findMin p) 0 empty) $ assocs $ p
    where 
    p = plan os
    calc :: (Money,Profit) -> (Time,[Bid]) -> (Money,Profit)
    calc (max,profit) (time,bids) = (bestValue,insert time bestValue profit)
        where 
        bestValue = maximum $ max:map value bids  

        value :: Bid -> Money
        value (price,start)  = price + (findWithDefault 0 start profit)

solutions :: [[Int]] -> [Int]
solutions = solutions' . tail 
    where
    solutions' :: [[Int]] -> [Int]
    solutions' [] = []
    solutions' ([n]:ns) = solve (take n ns) : solutions' (drop n ns) 
    
    solve :: [[Int]] -> Int
    solve = profit . map (\[s,d,p] -> (s,d,p))

process :: ByteString -> ByteString
process = output . solutions . input
    where
    input :: ByteString -> [[Int]]
    input = (map $ map (fst . fromJust . BS.readInt)).(map BS.words).BS.lines

    output :: [Int] -> ByteString
    output = BS.unlines . map (BS.pack . show)

