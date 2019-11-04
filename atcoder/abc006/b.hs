{-# LANGUAGE BangPatterns #-}

main :: IO ()
main = do
    n <- readLn
    print $ solve n

solve :: Int -> Int
solve n = go 0 0 1 !! (n - 1)
  where
    go !x !y !z = x : go y z (rem (x + y + z) 10007)