import Test.Hspec
import Rent
import qualified Data.Map as Map 
import Data.Map (toList,(!))
import Data.List

main = hspec $ do
    describe "order values" $ do
        it "should have a start time" $ do
            start (order 3 5 10) `shouldBe` 3 
        
        it "should have an end time" $ do
            end (order 3 5 10) `shouldBe` 8

        it "shoud have a price" $ do
            price (order 3 5 10) `shouldBe` 10

    describe "a plan" $ do
        it "should map time to order ending at that time" $ do
            let p = plan [order 0 5 10]
            p!5 `shouldContain` [(10,0)]

        it "should map several orders" $ do
            let p = plan [order 0 5 10, order 3 7 14]
            p!5  `shouldContain` [(10,0)]
            p!10 `shouldContain` [(14,3)]


        it "should map start times even if not order ending at that time" $ do
            let p = plan [order 0 2 10, order 3 2 14]
            toList p `shouldBe` [(0,[]),(2,[(10,0)]),(3,[]),(5,[(14,3)])]

        it "should hold the minimal start time" $ do
            let p = plan [order 0 5 10, order (-3) 7 14]
            minStartTime p `shouldBe` (-3)

    describe "a profit table" $ do
        it "should map time to money" $ do
            let t = fst $ profits $ plan [order 0 5 10]
            t!5 `shouldBe` 10
            toList t `shouldBe` [(0,0),(5,10)]

        it "should contain best profit as its max element" $ do
            let t = fst $ profits $ plan [order 0 5 10]
            toList t `shouldBe` [(0,0),(5,10)]

            let t = fst $ profits $ plan [order 0 5 10, order 3 2 14]
            toList t `shouldBe` [(0,0),(3,0),(5,14)]


            let t = fst $ profits $ plan [order 0 3 10, order 4 3 7]
            toList  (plan [order 0 3 10, order 4 3 7])  
                `shouldBe` [(0,[]),(3,[(10,0)]),(4,[]),(7,[(7,4)])]

            toList t `shouldBe` [(0,0),(3,10),(4,10),(7,17)]


            let t = fst $ profits $ plan [order 0 5 10
                                   ,order 3 7 14
                                   ,order 5 9 7
                                   ,order 6 9 8]
            toList t `shouldBe` [(0,0),(3,0),(5,10),(6,10),(10,14),(14,17),(15,18)]


    describe "profit" $ do
        it "should be the max value for a plan" $ do
            let os = [order 0 5 10
                     ,order 3 7 14
                     ,order 5 9 7
                     ,order 6 9 8]
            profit os `shouldBe` 18    

    context "process" $ do
        let ls = [[2]
                 ,[1]
                 ,[0,5,10]
                 ,[2]
                 ,[0,5,10]
                 ,[3,7,14]]
        describe "solutions" $ do
            it "should solve rent problems given as list of lists integers" $ do
                solutions ls `shouldBe` [10,14] 

        describe "process" $ do
            it "should read input and output solutions" $ do
                let s = "2 \n 1 \n 0 5 10 \n 2 \n 0 5 10 \n 3 7 14"
                process s `shouldBe` "10\n14\n"
                                    
