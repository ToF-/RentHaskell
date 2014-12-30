import Test.QuickCheck
import Rent

arbitraryOrder :: Gen Order
arbitraryOrder = do
    s <- elements [0..10]
    d <- elements [1..10]
    p <- elements [0..10]
    return $ order s d p 

arbitraryList :: Gen [Order]
arbitraryList = do
    n <- elements [1..5]
    sequence $ replicate n arbitraryOrder

oracle :: [Order] -> Money
oracle = profit
    where 
    profit [] = 0
    profit (o:os) = max
        (price o + profit (filter (after o) os))
        (profit os)   

    after o o' = end o <= start o' 

prop_correct_profit = forAll arbitraryList $ \os -> profit os == oracle os

main = do quickCheck prop_correct_profit

