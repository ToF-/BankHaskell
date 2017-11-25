module Bank where
import Data.List 

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
    perform ["D",d,n] acc out =  return ((d,read n,0,read n) : acc)
    perform ["W",d,n] acc out =  return ((d,0,read n,read n) : acc)
    perform ["S"] acc out = out (statement acc) >> return acc
    perform s _ out       = out ((unwords s) ++ "?") >> return acc 

    statement = unlines . (header :) . map showMouvement . foldr balance [] . reverse . sort
    
    header = " date | credit | debit | balance " 
    
    showMouvement (date,c,0,b) = " " ++ date ++ " | " ++ show c ++" | | " ++ show b ++ " " 
    showMouvement (date,0,d,b) = " " ++ date ++ " | | " ++ show d ++" | " ++ show b ++ " " 

    balance :: Operation -> [Operation] -> [Operation]
    balance (t,c,d,_) [] = [(t,c,d,c-d)]
    balance o' (o:os) = 
        let (_,_,_,b) = o
            (t,c,d,_) = o'
        in (t,c,d,b+c-d):o:os
    

