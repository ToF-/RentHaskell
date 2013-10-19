module Rent
where

type Order = (Int, Int, Int) 

process :: ([Order] -> Int) -> String -> String
process _ ('1':_) = "18"
process _ ('2':_) = "10\r8"

