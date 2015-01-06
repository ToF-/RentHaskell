import Test.Hspec
import Rent
import qualified Data.Map as Map 
import Data.Map (toList,(!))
import Data.List
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)

main = hspec $ do

    describe "a plan" $ do
        it "should map time to positions" $ do
            let p = plan [order 0 5 100, order 3 7 140, order 5 5 70] 
            toList p `shouldBe` [(0,[])
                                ,(3,[])
                                ,(5,[(100,0)])
                                ,(10,[(140,3),(70,5)])]

    describe "profit" $ do
        it "should be the max value for a plan" $ do
            let os = [order 0 5 100,order 3 7 140,order 5 9 70,order 6 9 80]
            profit (plan os) `shouldBe` 180

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


                                    
