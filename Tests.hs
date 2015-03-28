import Test.QuickCheck
import Rent
import Data.List (sort)

arbitraryOrder :: Gen Order
arbitraryOrder = do
    s <- elements [0..10]
    d <- elements [1..10]
    p <- elements [00..1000]
    return (s,d,p)

arbitraryList :: Gen [Order]
arbitraryList = do
    n <- elements [1..30]
    sequence $ replicate n arbitraryOrder

oracle :: [Order] -> Money
oracle = profit . sort
    where 
    profit [] = 0
    profit ((s,d,p):os) = max
        (p + profit (filter (after (s,d,p)) os))
        (profit os)   

    after (s,d,_) (s',_,_) = s+d <= s' 

prop_correct_profit = forAll arbitraryList $ \os -> profit os == oracle os

main = do quickCheck prop_correct_profit

