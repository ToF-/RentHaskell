import Test.Hspec
import Rent
import Data.Map 

main = hspec $ do
    describe "order values" $ do
        it "should have a start time" $ do
            start (order 3 5 10) `shouldBe` 3 
        
        it "should have an end time" $ do
            end (order 3 5 10) `shouldBe` 8

        it "shoud have a price" $ do
            price (order 3 5 10) `shouldBe` 10

    describe "time stamps from a list of orders" $ do
        it "should be all the distinct start and end time" $ do
            let os = [order 0 5 10, order 3 7 14, order 7 3 7]
            times os `shouldBe` [0,3,5,7,10]
            
    describe "null orders" $ do
        it "should link all times from a list of orders" $ do
            let os = [order 0 5 10, order 3 7 14, order 7 3 15]
            nullOrders os `shouldBe` [(0,3,0),(3,2,0),(5,2,0),(7,3,0)] 

    describe "a plan" $ do
        it "should map time to order ending at that time" $ do
            let p = plan [order 0 5 10]
            p!5 `shouldContain` [(10,0)]

        it "should map several orders" $ do
            let p = plan [order 0 5 10, order 3 7 14]
            p!5  `shouldContain` [(10,0)]
            p!10 `shouldContain` [(14,3)]

        it "should include null orders" $ do
            let p = plan [order 0 5 10, order 3 7 14, order 7 3 15]
            p!5 `shouldContain` [(0, 3)]
            p!7 `shouldContain` [(0, 5)]
            p!10`shouldContain` [(0, 7)]

        it "should hold the minimal start time" $ do
            let p = plan [order 0 5 10, order (-3) 7 14]
            minStartTime p `shouldBe` (-3)

    describe "a profit table" $ do
        it "should map time to money" $ do
            let t = profits $ plan [order 0 5 10]
            t!5 `shouldBe` 10

        it "should contain best profit as its max element" $ do
            let t = profits $ plan [order 0 5 10]
            toList t `shouldBe` [(0,0),(5,10)]

            let t = profits $ plan [order 0 5 10, order 3 2 14]
            toList t `shouldBe` [(0,0),(3,0),(5,14)]

            let t = profits $ plan [order 0 5 10
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

    describe "solve" $ do
        it "should solve rent problems given as list of lists integers" $ do
            let ls = [[2]
                     ,[1]
                     ,[0,5,10]
                     ,[2]
                     ,[0,5,10]
                     ,[3,7,14]]
            solutions ls `shouldBe` [10,14] 
            

                                    
