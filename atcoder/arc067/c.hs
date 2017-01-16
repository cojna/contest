{-# LANGUAGE BangPatterns #-}
import           Data.List

main=readLn >>= print.solve

solve :: Int -> Int
solve 1 = 1
solve n = foldl1'(\acc x->acc*x`mod`modulus).map (succ.length).group.sort $ concatMap primeFactors [2..n]

modulus :: Int
modulus = 1000000007

smallPrimes :: [Int]
smallPrimes = 2 : [ n | n<-[3,5..46337], all ((>0).rem n) $ takeWhile (\x->x*x<=n) smallPrimes]

primeFactors :: Int -> [Int]
primeFactors n | n < 2 = []
primeFactors n = go n smallPrimes
  where
    go !n pps@(p:ps)
        | n < p * p = [n|n>1]
        | r > 0     = go n ps
        | otherwise = p : go q pps
      where
        (q, r) = quotRem n p
    go n [] = [n]
