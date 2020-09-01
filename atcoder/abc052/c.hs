{-# LANGUAGE BangPatterns #-}

import qualified Data.List as L

import Debug.Trace

main :: IO ()
main = do
    n <- readLn
    print
        . L.foldl' (*%) 1
        . map (succ.length)
        . L.group
        . L.sort
        $ concatMap primeFactors [1..n]

(*%) :: Int -> Int -> Int
x *% y = x * y `rem` 1000000007

smallPrimes :: (Integral i) => [i]
smallPrimes = 2 : [ n | n<-[3,5..46337], all ((>0).rem n) $ takeWhile (\x->x*x<=n) smallPrimes]
{-# SPECIALIZE smallPrimes :: [Int] #-}

primeFactors :: (Integral i) => i -> [i]
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
{-# SPECIALIZE primeFactors :: Int -> [Int] #-}