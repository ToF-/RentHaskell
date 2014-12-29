import Test.Hspec
import Data.Map as Map (toList,keys)
import qualified Data.Map as Map 
import Rent

main = tests

os = [makeOrder 0 5 10
     ,makeOrder 3 7 14
     ,makeOrder 5 9 7
     ,makeOrder 6 9 8]        



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
        it "contains all time points except starting time" $ do
            Map.keys (plan os) `shouldBe` [3,5,6,10,14,15]
        

        it "maps time points to order ending at that point" $ do
            Map.toList (plan os) `shouldBe` 
                [(3,[Order {startTime = 0, endTime = 3, price = 0}])
                ,(5,[Order {startTime = 3, endTime = 5, price = 0}
                    ,Order {startTime = 0, endTime = 5, price = 10}])
                ,(6,[Order {startTime = 5, endTime = 6, price = 0}])
                ,(10,[Order {startTime = 6, endTime = 10, price = 0}
                     ,Order {startTime = 3, endTime = 10, price = 14}])
                ,(14,[Order {startTime = 10, endTime = 14, price = 0}
                     ,Order {startTime = 5, endTime = 14, price = 7}])
                ,(15,[Order {startTime = 14, endTime = 15, price = 0}
                    ,Order {startTime = 6, endTime = 15, price = 8}])]

        it "correctly maps points to order even with same starting points" $ do 
            let os = [makeOrder 1 4 16, makeOrder 1 9 13]
            Map.toList (plan os) `shouldBe`
                [(5,[Order {startTime = 1, endTime = 5, price = 0}
                    ,Order {startTime = 1, endTime = 5, price = 16}])
                ,(10,[Order {startTime = 5, endTime = 10, price = 0}
                     ,Order {startTime = 1, endTime = 10, price = 13}])]

    describe "exploit" $ do
        it "map time to money" $ do
            Map.toList (exploit (plan os)) `shouldBe`
                [(3,0),(5,10),(6,10),(10,14),(14,17),(15,18)]
        it "can solve a plan with 2 equal start time" $ do
            let os = [makeOrder 1 4 16, makeOrder 1 9 13]
            Map.toList (exploit (plan os)) `shouldBe`
                [(5,16),(10,16)]
        
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
        it "solves a series with identical start time" $ do
            let xs = [[2]
                     ,[1,4,16]
                     ,[1,9,13]]
            solve xs `shouldBe` [16]

    describe "process" $ do
        it "converts problem input into solution output" $ do
            let s = "2\n4\n0 5 10\n3 7 14\n5 9 7 \n6 9 8 \n3\n0 5 12\n3 7 14\n5 6 10"
            process s `shouldBe` "18\n22\n"
            let s = "1\n2\n15 4 6\n15 9 8"
            process s `shouldBe` "8\n"

