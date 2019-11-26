main = do
    n <- readLn
    cs <- getLine
    putStrLn $ [([c..'Z']++['A'..])!!n|c<-cs]