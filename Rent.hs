-- Rent.hs   all except the first 3 lines are copied to Spoj.hs
module Rent
where
import Data.Map (insertWith, findWithDefault, Map, empty)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)

type Order  = (Time, Time, Money)
type Time   = Int
type Money  = Int
data Operation   = Cash Time | Rent Time Time Money deriving(Eq,Ord,Show)
type Schedule = [Operation]
type Profits = Map Time Money

profit :: [Order] -> Money
profit = fst . foldl calc (0, empty) . schedule
    where
    calc :: (Money, Profits) -> Operation -> (Money, Profits)
    calc (v, profits) (Rent t d p) = (v, insertWith max (t+d) (v+p) profits)
    calc (v, profits) (Cash t    ) = (max v (findWithDefault 0 t profits), profits)

schedule :: [Order] -> Schedule
schedule os = sortBy (comparing time) $ (map cash os) ++ (map rent os)
    where
    rent (start,duration,price) = Rent start duration price

    cash (start,duration,_)     = Cash (start+duration)

    time (Cash t) = t
    time (Rent t _ _) = t

solutions :: [[Int]] -> [Int]
solutions = solutions' . tail 
    where
    solutions' [] = []
    solutions' ([n]:ns) = solve (take n ns) : solutions' (drop n ns) 
    
    solve = profit . map (\[s,d,p] -> (s,d,p))

process :: ByteString -> ByteString
process = output . solutions . input
    where
    input = (map $ map (fst . fromJust . BS.readInt)).(map BS.words).BS.lines

    output = BS.unlines . map (BS.pack . show)

