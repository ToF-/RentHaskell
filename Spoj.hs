
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

type Flight = (Money,Time)
type Position = [Flight]
type Plan = Map Time Position

plan :: [Order] -> Plan
plan = fromListWith (++) . foldl positions []
    where

    positions :: [(Time,Position)] -> Order -> [(Time,Position)]
    positions l o = start o : arrival o : l  

    arrival :: Order -> (Time,Position)
    arrival (s,d,p) = (s+d,[(p,s)])
 
    start :: Order -> (Time,Position)
    start (s,_,p) = (s,[])

type Profit = Map Time Money

profit :: Plan -> Money
profit = fst . evaluate  
    where 
    evaluate :: Plan -> (Money,Profit)
    evaluate p = foldl calc (0, insert (fst $ findMin p) 0 empty) $ assocs p 

    calc :: (Money,Profit) -> (Time,Position) -> (Money,Profit)
    calc (max,plan) (time,position) = (bestValue,insert time bestValue plan)
        where 
        bestValue = maximum $ max:map value position  

        value :: Flight -> Money
        value (price,start)  = price + (findWithDefault 0 start plan)

solutions :: [[Int]] -> [Int]
solutions = solutions' . tail 
    where
    solutions' :: [[Int]] -> [Int]
    solutions' [] = []
    solutions' ([n]:ns) = solve (take n ns) : solutions' (drop n ns) 
    
    solve :: [[Int]] -> Int
    solve = profit . plan . map (\[s,d,p] -> order s d p)

process :: ByteString -> ByteString
process = output . solutions . input
    where
    input :: ByteString -> [[Int]]
    input = (map $ map (fst . fromJust . BS.readInt)).(map BS.words).BS.lines

    output :: [Int] -> ByteString
    output = BS.unlines . map (BS.pack . show)

main = BS.interact process
