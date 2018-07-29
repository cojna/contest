{-# LANGUAGE BangPatterns #-}

import qualified Data.Foldable as F

main :: IO ()
main = getLine >>= print.solve.map read.words

solve :: [Integer] -> Integer
solve [c, d] = F.foldl' step 0 kenkoooo
  where
    step acc (l, r)
      | d - 1 < l || r < c = acc
      | otherwise = acc + min (d - 1) r - max c l + 1

kenkoooo :: [(Integer, Integer)]
kenkoooo = take 64 $ iterate step (140, 169)
  where
    step (!l, !r) = (2 * l, 2 * r + 1)
