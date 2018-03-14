main :: IO ()
main = do
    a <- readLn
    [b, c] <- map read . words <$> getLine
    cs <- getLine
    putStrLn $ shows (sum [a, b, c]) " " ++ cs
