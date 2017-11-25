module Bank where

type Account = [Operation]
type Operation = (Date, Credit, Debit, Balance)
type Date   = String
type Credit = Double
type Debit  = Double
type Balance= Double

empty :: Account 
empty = []

command :: Monad m => Account -> (m String) -> (String -> m ()) -> m Account
command acc inp out = inp >>= \s -> perform (words s) acc out
    where
    perform :: Monad m => [String] -> Account -> (String -> m ()) -> m Account
    perform ["S"] acc out = out (statement acc) >> return acc
    perform s _ out       = out ((unwords s) ++ "?") >> return acc 

    statement = unlines . (header :) . map showOp
    
    header = " date | credit | debit | balance " 
    
    showOp (date,c,0,b) = " " ++ date ++ " | " ++ show c ++" | | " ++ show c ++ " " 
    showOp (date,0,d,b) = " " ++ date ++ " | | " ++ show d ++" | " ++ show (-d) ++ " " 
    

