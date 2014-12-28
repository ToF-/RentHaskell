import Test.Hspec

testing = False 
-- True: pass tests. False: run program
-- passing an arg won't do because hspec wants to consume the arg too

main = if testing then tests else process

process :: IO ()
process = putStrLn "18"

tests :: IO ()
tests = hspec $ do
    describe "test" $ do
        it "should pass" $ do
            pendingWith "to do"
