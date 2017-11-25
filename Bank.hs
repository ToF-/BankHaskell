module Bank where

type Account = [Operation]
type Operation = (Date, Credit, Debit, Balance)
type Date   = String
type Credit = Double
type Debit  = Double
type Balance= Double

emptyAccount :: Account 
emptyAccount = []

printStatement :: Monad m => Account -> (String -> m ()) -> m ()
printStatement acc output = output $ unlines $ header : statement acc 
    where 
    statement = map showOperation
    header = " date | credit | debit | balance " 
    showOperation (date,c,0,b) = " " ++ date ++ " | " ++ show c ++" | | " ++ show c ++ " " 
    showOperation (date,0,d,b) = " " ++ date ++ " | | " ++ show d ++" | " ++ show (-d) ++ " " 

getOperation :: Monad m => Account -> (m String) -> m Account
getOperation acc input = input >>= \s -> perform (words s) acc  

perform :: Monad m => [String] -> Account -> m Account
perform [oper,date,amount] acc = case head oper of
    'W' -> return ((date, 0, read amount, negate (read amount)) : acc)
    'D' -> return ((date, read amount, 0, read amount) : acc)
perform _ acc = return acc
