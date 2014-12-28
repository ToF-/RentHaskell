import System.Random (getStdRandom, randomR)
import System.Environment (getArgs)

{-- runghc GenerateTestFile M 
 -      generates 30 test cases of 10000 orders
 -  runghc GenerateTestFile
 -      generates m test cases of n orders
--}

main = do
    args <- getArgs 
    d <- randomTestData (args == ["M"])
    putStr $ unlines $ showTestData d

randomInt :: (Int, Int) -> IO Int
randomInt = getStdRandom . randomR

randomTime = randomInt (0, 1000000)
randomPrice = randomInt (1, 100000)
{--
The start time of the order st (0 ≤ st < 1000000),
the duration d of the order (0 < d < 1000000),
and the price p (0 < p < 100000) the customer
is ready to pay for this order.
 --}
type Order = (Int, Int, Int)
type Problem = (Int, [Order])
type TestData = (Int, [Problem])

randomOrder :: IO Order 
randomOrder = do
    s <- randomTime
    d <- randomTime
    p <- randomPrice
    return (s,d,p)

randomProblem :: Bool -> IO Problem
randomProblem maxed = do
    n <- if maxed then return 10000 else randomInt (1,10001)
    os <- sequence $ replicate n randomOrder
    return (n,os)

randomTestData :: Bool -> IO TestData
randomTestData maxed = do 
    n <- if maxed then return 30 else randomInt (1,31)
    ps <- sequence $ replicate n $ randomProblem maxed
    return (n,ps)

showOrder :: Order -> String
showOrder (s,d,p) = show s ++" " ++ show d ++ " " ++ show p

showProblem :: Problem -> [String]
showProblem (n,os) = show n : map showOrder os

showTestData :: TestData -> [String] 
showTestData (n,ps) = show n : concatMap showProblem ps   

