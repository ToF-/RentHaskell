import Test.Hspec
import Data.Map as Map (toList,keys)
import qualified Data.Map as Map 
import Rent

main = tests

tests :: IO ()
tests = hspec $ do
    describe "an order" $ do
        it "should have a start time" $ do
            startTime (makeOrder 3 5 10) `shouldBe` 3
        it "should have an end time" $ do
            endTime (makeOrder 3 5 10) `shouldBe` 3+5
        it "should have a price" $ do
            price (makeOrder 3 5 10) `shouldBe` 10

    describe "a plan" $ do
        it "contains end time points" $ do
            Map.keys (plan os) `shouldBe` [3,5,6,10,14,15]

        it "maps time points to order ending at that point" $ do
            Map.toList (plan os) `shouldBe` 
                [(3,[Order {startTime = 0, endTime = 3, price = 0}])
                ,(5,[Order {startTime = 0, endTime = 5, price = 10}
                    ,Order {startTime = 3, endTime = 5, price = 0}])
                ,(6,[Order {startTime = 5, endTime = 6, price = 0}])
                ,(10,[Order {startTime = 3, endTime = 10, price = 14}])
                ,(14,[Order {startTime = 5, endTime = 14, price = 7}])
                ,(15,[Order {startTime = 6, endTime = 15, price = 8}])]

    describe "exploit" $ do
        it "map time to money" $ do
            Map.toList (exploit (plan os)) `shouldBe`
                [(3,0),(5,10),(6,10),(10,14),(14,17),(15,18)]
        
    describe "profit" $ do
        it "optimizes orders" $ do
            profit os `shouldBe` 18
   
    describe "solve" $ do
        it "solves a series of profit problems" $ do 
            let xs = [[4]
                     ,[0,5,10]
                     ,[3,7,14]
                     ,[5,9,7 ]
                     ,[6,9,8 ]
                     ,[3]
                     ,[0,5,12]
                     ,[3,7,14]
                     ,[5,6,10]]
            solve xs `shouldBe` [18,22]

    describe "process" $ do
        it "converts problem input into solution output" $ do
            let s = "2\n4\n0 5 10\n3 7 14\n5 9 7 \n6 9 8 \n3\n0 5 12\n3 7 14\n5 6 10"
            process s `shouldBe` "18\n22\n"
