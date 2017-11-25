module Bank where

type Account = [Operation]
type Operation = (Date, Credit, Debit, Balance)
type Date   = String
type Credit = Double
type Debit  = Double
type Balance= Double

empty :: Account 
empty = []

operation :: Monad m => Account -> (m String) -> (String -> m ()) -> m Account
operation acc inp out = inp >>= \s -> perform (words s) acc out
    where
    perform :: Monad m => [String] -> Account -> (String -> m ()) -> m Account
    perform ["S"] acc out = output acc out >> return acc
    perform s _ out       = out ((unwords s) ++ "?") >> return acc 

    output :: Monad m => Account -> (String -> m ()) -> m ()
    output acc out = out $ unlines $ header : statement acc

        where 
        statement = map showOperation
        header = " date | credit | debit | balance " 
        showOperation (date,c,0,b) = " " ++ date ++ " | " ++ show c ++" | | " ++ show c ++ " " 
        showOperation (date,0,d,b) = " " ++ date ++ " | | " ++ show d ++" | " ++ show (-d) ++ " " 


