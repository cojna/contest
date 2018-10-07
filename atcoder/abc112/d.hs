{-# LANGUAGE BangPatterns #-}

import qualified Data.List as L

main :: IO ()
main = getLine >>= print . solve . map read . words

solve :: [Int] -> Int
solve [n, m] = maximum [g | g<-divisors m, div m g >= n]

smallPrimes :: [Int]
smallPrimes = 2 : [ n | n<-[3,5..46337], all ((>0).rem n) $ takeWhile (\x->x*x<=n) smallPrimes]

primeFactors :: Int -> [Int]
primeFactors n | n < 2 = []
primeFactors n = go n smallPrimes
  where
    go !n pps@(p:ps)
        | n < p * p = [n]
        | r > 0     = go n ps
        | otherwise = p : go q pps
      where
        (q, r) = quotRem n p
    go n [] = [n]

divisors :: Int -> [Int]
divisors n = L.sort . map product . mapM (scanl (*) 1) . L.group $ primeFactors n
