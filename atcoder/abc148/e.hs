main :: IO ()
main = do
    x <- readLn
    case divMod x 2 of
        (_, 1) -> print 0
        (q, 0) -> print $ sum [div q (5 ^ i)|i<-[1..64]]
