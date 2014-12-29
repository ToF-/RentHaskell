import System.Random (getStdRandom, randomR)
import System.Environment (getArgs)
import Control.Monad (replicateM)

{-- runghc GenerateTestFile M 
 -      generates 30 test cases of 10000 orders
 -  runghc GenerateTestFile
 -      generates m test cases of n orders
--}

main = do
    args <- getArgs 
    let size = sizeFromArgs args
    d <- randomTestData (args == ["M"]) size
    putStr $ unlines $ showTestData d

sizeFromArgs :: [String] -> (Int,Int)
sizeFromArgs [] = (30, 10000)
sizeFromArgs ["M"] = (0,0)
sizeFromArgs ["S",n,m] = (read n,read m)

{--
The start time of the order st (0 ≤ st < 1000000),
the duration d of the order (0 < d < 1000000),
and the price p (0 < p < 100000) the customer
is ready to pay for this order.
--}

randomInt :: (Int, Int) -> IO Int
randomInt = getStdRandom . randomR

randomTime = randomInt (0, 1000000)
randomPrice = randomInt (1, 100000)

type Order = (Int, Int, Int)
type Problem = (Int, [Order])
type TestData = (Int, [Problem])

randomOrder :: IO Order 
randomOrder = do
    s <- randomTime
    d <- randomTime
    p <- randomPrice
    return (s,d,p)

randomProblem :: Bool -> Int -> IO Problem
randomProblem maxed maxOrders = do
    n <- if maxed then return 10000 else randomInt (1,maxOrders)
    os <- replicateM n randomOrder
    return (n,os)

randomTestData :: Bool -> (Int,Int) -> IO TestData
randomTestData maxed (nbTests,maxOrders) = do 
    n <- if maxed then return 30 else randomInt (1,nbTests)
    ps <- replicateM n $ randomProblem maxed maxOrders
    return (n,ps)

showOrder :: Order -> String
showOrder (s,d,p) = show s ++" " ++ show d ++ " " ++ show p

showProblem :: Problem -> [String]
showProblem (n,os) = show n : map showOrder os

showTestData :: TestData -> [String] 
showTestData (n,ps) = show n : concatMap showProblem ps   

