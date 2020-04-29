import qualified Data.List as L

main :: IO ()
main = do
    n <- readLn
    print $ solve n

solve :: Int -> Int
solve n = length $ filter isValid ds0 ++ ds1
  where
    ds0 = filter (>1) $ divisors n
    ds1 = filter (>1) $ divisors (n - 1)
    isValid d = simulate n d == 1

    simulate x k = case quotRem x k of
        (q, 0) -> simulate q k
        (q, r) -> r

primeFactors :: (Integral i) => i -> [i]
primeFactors n | n < 2 = []
primeFactors n = go n [2..1000000]
  where
    go n pps@(p:ps)
        | n < p * p = [n]
        | r > 0     = go n ps
        | otherwise = p : go q pps
      where
        (q, r) = quotRem n p
    go n [] = [n]
{-# SPECIALIZE primeFactors :: Int -> [Int] #-}

divisors :: Int -> [Int]
divisors n = L.sort . map product . mapM (scanl (*) 1) . L.group $ primeFactors n
