import Test.Hspec

testing = True 
-- True: pass tests. False: run program
-- passing an arg won't do because hspec wants to consume the arg too

main = if testing then tests else process

process :: IO ()
process = putStrLn "18"

tests :: IO ()
tests = hspec $ do
    describe "an order" $ do
        it "should have a start time" $ do
            startTime (makeOrder 3 5 10) `shouldBe` 3
        it "should have an end time" $ do
            endTime (makeOrder 3 5 10) `shouldBe` 3+5
        it "should have a price" $ do
            price (makeOrder 3 5 10) `shouldBe` 10

data Order = Order { startTime :: Int, endTime :: Int, price :: Int }

makeOrder :: Int -> Int -> Int -> Order
makeOrder s d p = Order s (s+d) p 

