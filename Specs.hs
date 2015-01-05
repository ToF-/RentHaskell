import Test.Hspec
import Rent
import qualified Data.Map as Map 
import Data.Map (toList,(!))
import Data.List
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)

main = hspec $ do
    describe "order values" $ do
        it "should have a start time" $ do
            start (order 3 5 100) `shouldBe` 3 
        
        it "should have an end time equal to start+duration" $ do
            end (order 3 5 100) `shouldBe` 8

        it "shoud have a price" $ do
            price (order 3 5 100) `shouldBe` 100

    describe "a plan" $ do
        it "should map time to orders ending at that time" $ do
            let p = plan [order 0 5 100, order 3 2 140]
            p `shouldSatisfy` (Map.member 0)
            p `shouldSatisfy` (Map.member 3)
            
        it "should hold the minimal start time" $ do
            let p = plan [order 0 5 10, order (-3) 7 14]
            minStartTime p `shouldBe` (-3)

    describe "a plan'" $ do
        it "should map time to positions" $ do
            let p = plan' [order 0 5 100, order 3 7 140, order 5 5 70] 
            toList p `shouldBe` [(0,(0,[]))
                                ,(3,(0,[]))
                                ,(5,(0,[(100,0)])),
                                (10,(0,[(140,3),(70,5)]))]
    describe "a profit'" $ do
        it "should contain best profit in last position" $ do
            let p = plan' [order 0 5 100,order 3 7 140,order 5 9 70,order 6 9 80]
            profit' p `shouldBe` 180

    describe "a profit table" $ do
        let pt = toList . fst . profits . plan 

        it "should map time to best profit at that time" $ do
            pt [order 0 5 100]
            `shouldBe` [(0,0),(5,100)]

        it "should contain best profit given orders on same times" $ do
            pt [order 0 5 100, order 0 5 140]
            `shouldBe` [(0,0),(5,140)]

        it "should contain best profit in last position" $ do
            pt [order 0 3 140, order 0 4 100] 
           `shouldBe` [(0,0),(3,140),(4,140)]

        it "should contain best profit in last position" $ do
            pt [order 0 5 100,order 3 7 140,order 5 9 70,order 6 9 80]
           `shouldBe`[(0,0),(3,0),(5,100),(6,100),(10,140),(14,170),(15,180)]

        it "should contain best profit given overlapping orders" $ do
            pt [order 0 5 100, order 3 2 140]
            `shouldBe` [(0,0),(3,0),(5,140)]

        it "should contain best profit given non overlapping orders" $ do
            pt [order 0 5 100, order 6 2 140]
            `shouldBe` [(0,0),(5,100),(6,100),(8,240)]

    describe "profit" $ do
        it "should be the max value for a plan" $ do
            let os = [order 0 5 100,order 3 7 140,order 5 9 70,order 6 9 80]
            profit os `shouldBe` 180

    context "process" $ do
        let ls = [[2]
                 ,[1]
                 ,[0,5,100]
                 ,[1]
                 ,[3,7,140]]
        describe "solutions" $ do
            it "should solve rent problems given as list of lists integers" $ do
                solutions ls `shouldBe` [100,140] 

        describe "process" $ do
            it "should read input and output solutions" $ do
                let s = BS.pack "2 \n 1 \n 0 5 100 \n 1 \n 3 7 140 "
                BS.unpack (process s) `shouldBe` "100\n140\n"


                                    
