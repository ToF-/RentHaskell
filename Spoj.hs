import Data.Map (insertWith, findWithDefault, Map, empty)
import Data.List (sort)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)

type Order  = (Time, Time, Money)
type Time   = Int
type Money  = Int
data Slot   = Cash | Rent (Time, Money)
    deriving(Eq,Ord,Show)
type Schedule = [(Time,Slot)]
type Profit = Map Time Money

profit :: [Order] -> Money
profit = fst . foldl calc (0, empty) . schedule
    where
    calc :: (Money, Profit) -> (Time, Slot) -> (Money, Profit)
    calc (value, profits) (_, Rent (end, price)) = (value, insertWith max end (value+price) profits)
    calc (value, profits) (time, Cash)           = (max value (findWithDefault 0 time profits), profits)

schedule :: [Order] -> Schedule
schedule os = sort $ (map bid os) ++ (map slot os)
    where
    bid :: Order -> (Time, Slot)
    bid (start,duration,price) = (start, Rent (start+duration,price))

    slot :: Order -> (Time, Slot) 
    slot (start,duration,_) = (start+duration, Cash)


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

main = BS.interact process
