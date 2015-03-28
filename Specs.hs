import Test.Hspec
import Rent
import qualified Data.Map as Map 
import Data.Map (toList,(!))
import Data.List
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)

main = hspec $ do

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

        describe "schedule" $ do
            it "should collect times with maybe orders" $ do
                let os = [(0, 7, 1000)
                         ,(3, 5, 1400)
                         ,(7, 7,  800)
                         ,(8, 7,  900)]
                schedule os `shouldBe` 
                    [(00, Just (7,1000))
                    ,(03, Just (8,1400))
                    ,(07, Nothing)
                    ,(07, Just (14, 800))
                    ,(08, Nothing)
                    ,(08, Just (15, 900))
                    ,(14, Nothing)
                    ,(15, Nothing)]

    describe "a plan" $ do
        it "should map time to profit" $ do
            let p = plan [(0,5,100),(3,7,140),(5,5,70)] 
            toList (snd p) `shouldBe` 
                [(5,100) ,(10,170)]

    describe "profit" $ do
        it "should be the max value for a plan" $ do
            let os = [(0,5,100),(3,7,140),(5,9,70),(6,9,80)]
            profit os `shouldBe` 180
                

                                    
