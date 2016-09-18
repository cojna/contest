{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns #-}

main :: IO ()
main = readLn >>= putStr.unlines.map show.solve

solve :: Int -> [Integer]
solve n = take n $ go 1 2
  where
    go !k !x = a : go (k + 1) (k * (k + 1))
      where
        a = k * (k + 1) * (k + 1) - quot x k
