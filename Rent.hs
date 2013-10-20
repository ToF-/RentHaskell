-- Rent.hs
module Rent
where
import Data.List (sort)
type Time = Int
type Duration = Int
type Price = Int
type Order = (Time, Duration, Price) 

price :: Order -> Price
price (_,_,p) = p

after :: Order -> Order -> Bool
after (s,d,_) (s',d',_) = s' >= s+d

optimizeL :: [(Order)] -> Int
optimizeL = solution . sort 
    where solution [] = 0
          solution (o:os) = max (price o + solution (filter (after o) os)) (solution os)

process :: ([Order] -> Int) -> String -> String
process optimize =  output . map optimize . extract . input . tail
    where output = unlines . map show
          input  = map (map read) . map words . lines

extract :: [[Int]] -> [[Order]]
extract ns = extract' lengths orders
    where lengths = concat $ filter isLength ns
          orders  = map fromInts $ filter isOrder ns
          isLength = (1 ==).length
          isOrder  = (3 ==).length
          fromInts [s,d,p] = (s,d,p)
          extract' [] _      = []
          extract' (n:ns) os = (take n os) : extract' ns (drop n os) 




