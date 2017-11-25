import Test.Hspec
import Control.Monad.Writer
import Bank

main = hspec $ do
    describe "operation" $ do
        describe "S prints the account statement" $ do
            it "when empty account, prints just the header" $ do
                let out = \s -> writer ((), s)
                    inp = return "S"
                    run = operation empty inp out 
                lines (snd (runWriter run)) `shouldBe` [" date | credit | debit | balance "] 

        describe "prints an error message" $ do
            it "when no command or unknown command" $ do
                let out = \s -> writer ((), s)
                    inp = return "Foo Bar"
                    run = operation empty inp out
                lines (snd (runWriter run)) `shouldBe` ["Foo Bar?"]

{--
    describe "printStatement" $ do
        describe "should output the account statement" $ do
            it "for an empty account" $ do
                let acc = emptyAccount
                    out = \s -> writer ((), s) 
                    w    = printStatement acc out
                snd (runWriter w) `shouldBe` 

    describe "getOperation" $ do
        describe "should get an operation and perform it on the account" $ do
            it "for a deposit" $ do
                acc <- getOperation emptyAccount (return "D 2017/25/01 48.07")
                let out = \s -> writer ((), s) 
                    w    = printStatement acc out
                lines (snd (runWriter w)) `shouldBe` 
                        [" date | credit | debit | balance "
                        ," 2017/25/01 | 48.07 | | 48.07 "]
            it "for a withdrawal" $ do
                acc <- getOperation emptyAccount (return "W 2017/25/01 48.07")
                let out = \s -> writer ((), s) 
                    w    = printStatement acc out
                lines (snd (runWriter w)) `shouldBe` 
                        [" date | credit | debit | balance "
                        ," 2017/25/01 | | 48.07 | -48.07 "]
--}

                        
                
            
