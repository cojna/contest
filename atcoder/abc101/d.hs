{-# LANGUAGE BangPatterns #-}

import Control.Monad
import Data.Char
import Data.Maybe

main :: IO ()
main = do
  k <- readLn
  putStr.unlines.map show . take k $ iterate (nextSnuke . succ) 1

calc :: Int -> Double
calc n = fromIntegral n / fromIntegral (digitSum n)

digitSum :: Int -> Int
digitSum = sum . map digitToInt . show

nextSnuke :: Int -> Int
nextSnuke x = go (digitSum x) x
  where
    digits = iterate (*10) 1
    go s n = case listToMaybe cands of
        Nothing -> n
        Just (!dn, !ds) -> go (s + ds) (n + dn)
      where
        p dn ds = dn * s < n * ds
        cands = do
          (d, x) <- zip digits
            $ map digitToInt . reverse $ show n
          guard $ x /= 9 && p d 1
          return (d, 1)
