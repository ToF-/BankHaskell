import Bank


process :: Account -> IO ()
process acc = do
    acc' <- command acc getLine putStrLn
    process acc'

main = process empty
