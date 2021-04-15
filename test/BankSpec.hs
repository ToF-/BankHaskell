module BankSpec
    where

import Test.Hspec
import Control.Monad.Writer
import Bank

spec :: SpecWith ()
spec = do
    describe "command" $ do
        describe "S prints the account statement" $ do
            it "when empty account, prints just the header" $ do
                let out = \s -> writer ((), s)
                    inp = return "S"
                    run = command empty inp out 
                lines (snd (runWriter run)) `shouldBe` [" date | credit | debit | balance "] 

        describe "prints an error message" $ do
            it "when no command or unknown command" $ do
                let out = \s -> writer ((), s)
                    inp = return "Foo Bar"
                    run = command empty inp out
                lines (snd (runWriter run)) `shouldBe` ["Foo Bar?"]

        describe "D makes a deposit" $ do
            it "when it's the first deposit, balance equals amount" $ do
                let out = \s -> writer ((), s)
                    inp1 = return "D 2017/11/25 42.07"
                    inp2 = return "S"
                    run = command empty inp1 out >>= \acc -> command acc inp2 out
                lines (snd (runWriter run)) `shouldBe` 
                    [" date | credit | debit | balance "
                    ," 2017/11/25 | 42.07 | | 42.07 "] 

            it "when it's the second deposit, balance equals report+deposit" $ do
                let out = \s -> writer ((), s)
                    inp1 = return "D 2017/11/23 42.07"
                    inp2 = return "D 2017/11/25 42.07"
                    inp3 = return "S"
                    run = command empty inp1 out 
                            >>= \acc -> command acc inp2 out 
                            >>= \acc -> command acc inp3 out
                lines (snd (runWriter run)) `shouldBe` 
                    [" date | credit | debit | balance "
                    ," 2017/11/25 | 42.07 | | 84.14 " 
                    ," 2017/11/23 | 42.07 | | 42.07 "] 

        describe "W makes a withdrawal" $ do
            it "when it's the first withdrawal, balance equals amount" $ do
                let out = \s -> writer ((), s)
                    inp1 = return "W 2017/11/25 42.07"
                    inp2 = return "S"
                    run = command empty inp1 out >>= \acc -> command acc inp2 out
                lines (snd (runWriter run)) `shouldBe` 
                    [" date | credit | debit | balance "
                    ," 2017/11/25 | | 42.07 | -42.07 "] 
                

{--
    describe "printStatement" $ do
        describe "should output the account statement" $ do
            it "for an empty account" $ do
                let acc = emptyAccount
                    out = \s -> writer ((), s) 
                    w    = printStatement acc out
                snd (runWriter w) `shouldBe` 

    describe "getOperation" $ do
        describe "should get an command and perform it on the account" $ do
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

                        
                
            
