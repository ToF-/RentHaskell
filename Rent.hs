module Rent
where

type Time = Int
type Duration = Int
type Price = Int
type Order = (Time, Duration, Price) 

process :: ([Order] -> Int) -> String -> String
process optimize =  output . map optimize . extract . input . tail
    where output = unlines . map show
          input  = map (map read) . map words . lines

extract :: [[Int]] -> [[Order]]
extract ns = let
    lengths = concat $ filter isLength ns
    orders  = map fromInts $ filter isOrder ns
    in extract' lengths orders 

isLength :: [Int] -> Bool
isLength = (1 ==).length

isOrder :: [Int] -> Bool
isOrder = (3 ==).length

fromInts :: [Int] -> Order
fromInts [s,d,p] = (s,d,p)

extract' :: [Int] -> [Order] -> [[Order]]
extract' [] _  = []
extract' (n:ns) os = (take n os) : extract' ns (drop n os) 



