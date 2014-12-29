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
