main :: IO ()
main = do
    n <- readLn
    let odds = filter odd [1..n]
    print $ fromIntegral (length odds) / fromIntegral n