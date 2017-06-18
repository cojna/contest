{-# LANGUAGE BangPatterns #-}
import           Debug.Trace
main=getLine>>=print.solve
solve :: String -> Int
solve cs = minimum $ map step ['a'..'z']
  where
    step c = go 0 cs
      where
        go !res xs
          | all (c==) xs = res
          | otherwise = go (res + 1) . init $ f xs
        f (x:y:xs)
          | x == c || y == c = c : f (y:xs)
          | otherwise = x : f (y:xs)
        f xs = xs
