{-# LANGUAGE BangPatterns #-}

import           Control.Applicative
import           Control.Monad
import           Control.Parallel.Strategies
import           Data.Bits
import           Data.Char
import           Data.Fixed
import           Data.Int
import           Data.List
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Debug.Trace
import           System.IO

solve :: Task -> Answer
solve = solveLarge

solveLarge :: Task -> Answer
solveLarge (32, 500) = take j
    [ (jamcoin * 10 ^ (div n 2) + jamcoin, map (head.primeFactors.flip toBase digits) [2..10])
    | x<-map (+1) [2^(div n 2 - 1), 2^(div n 2 - 1)+2..]
    , let digits = toDigits x
    , isJamcoin digits
    , let jamcoin = toBase 10 digits
    ]
  where
    n = 32
    j = 500
    toDigits :: Integer -> [Integer]
    toDigits x = map (fromIntegral.fromEnum.testBit x) [div n 2-1,div n 2-2..0]
    toBase b = foldl' (\acc y->acc*b+y) 0
    isJamcoin digits = all (not.isPrime.flip toBase digits) [2..10]

solveSmall :: Task -> Answer
solveSmall (16, 50) = take j
    [ (jamcoin, map (head.primeFactors.flip toBase digits) [2..10])
    | x<-map (+1) [2^(n-1),2^(n-1)+2..]
    , let digits = toDigits x
    , isJamcoin digits
    , let jamcoin = toBase 10 digits
    ]
  where
    n = 16
    j = 50
    toDigits :: Integer -> [Integer]
    toDigits x = map (fromIntegral.fromEnum.testBit x) [n-1,n-2..0]
    toBase b = foldl' (\acc y->acc*b+y) 0
    isJamcoin digits = all (not.isPrime.flip toBase digits) [2..10]


smallPrimes :: [Integer]
smallPrimes = 2 : [ n | n<-[3,5..], all ((>0).rem n) $ takeWhile (\x->x*x<=n) smallPrimes]

primeFactors :: Integer -> [Integer]
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

isPrime :: Integer -> Bool
isPrime n = [n] == primeFactors n

type Task = (Int, Int)
getTask :: IO Task
getTask = do
    [n, j] <- map read.words <$> getLine
    return (n, j)

type Answer = [(Integer, [Integer])]
putAnswer :: Answer -> IO ()
putAnswer = putStr.unlines.map(unwords.map show.uncurry (:))

main :: IO ()
main = do
  t <- readLn
  start <- getPOSIXTime
  answers <- parMap rdeepseq solve <$> replicateM t getTask
  foldM_ `flip` start `flip` (zip [1..] answers)$ \prev (i, answer) -> do
      putStrLn $ "Case #" ++ shows i ":"
      putAnswer answer
      cur <- getPOSIXTime
      hPutStr stderr $ shows i "/" ++ shows t ": "
      hPutStrLn stderr $ (shows.msec) (cur - prev) "ms"
      return cur

msec :: NominalDiffTime -> Int
msec s = let t = realToFrac s :: Milli in fromEnum t
