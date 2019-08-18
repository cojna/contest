{-# LANGUAGE BangPatterns #-}

main :: IO ()
main = do
    cs <- getLine
    print $ solve cs

solve :: String -> Int
solve = go 0
  where
    go !acc (x:y:z:xs)
        | x == y = go (acc + 2) xs
        | otherwise = go (acc + 1) (y:z:xs)
    go acc [x, y]
        | x == y = acc + 1
        | otherwise = acc + 2
    go acc [x] = acc + 1
    go acc [] = acc
