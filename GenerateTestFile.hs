import System.Random

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

randomOrder :: IO (Int, Int, Int) 
randomOrder = do
    s <- randomTime
    d <- randomTime
    p <- randomPrice
    return (s,d,p)

main = do o <- randomOrder
          print o
