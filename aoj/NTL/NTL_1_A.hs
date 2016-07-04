{-# LANGUAGE BangPatterns #-}

main :: IO ()
main = do
    n <- readLn
    putStrLn $ shows n ": " ++ unwords (map show $ primeFactors n)

smallPrimes :: [Int]
smallPrimes=2:[n|n<-[3,5..46337],and.map((0<).rem n)$takeWhile(\x->x*x<=n)smallPrimes]

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
