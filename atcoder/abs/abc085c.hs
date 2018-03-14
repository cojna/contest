main :: IO ()
main = do
    [n, y] <- map read . words <$> getLine
    case [(a, b, n - a - b)|a<-[0..n], b<-[0..n-a], 10000 * a + 5000 * b + 1000 * (n - a - b) == y] of
        (a, b, c):_ -> putStrLn . unwords $ map show [a, b, c]
        _           -> putStrLn "-1 -1 -1"
