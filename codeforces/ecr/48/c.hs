{-# LANGUAGE BangPatterns #-}

import           Control.Applicative
import qualified Data.ByteString.Char8 as B
import           Data.Char
import           Data.Int
import qualified Data.List             as L

main :: IO ()
main = do
    n <- readLn
    xs <- map fromIntegral.L.unfoldr (B.readInt.B.dropWhile isSpace) <$> B.getLine
    ys <- map fromIntegral.L.unfoldr (B.readInt.B.dropWhile isSpace) <$> B.getLine
    print $ solve n xs ys

solve :: Int64 -> [Int64] -> [Int64] -> Int64
solve n xs ys = go 0 0 0 0 upperWeightedSum lowerWeightedSum $ zip xs ys
  where
    !totalSum = sum xs + sum ys
    ixs = zipWith (*) [0..] xs
    iys = zipWith (*) [0..] ys
    irxs = zipWith (*) [n..] $ reverse xs
    irys = zipWith (*) [n..] $ reverse ys
    upperWeightedSum = reverse . scanl1 (+) . map (uncurry(+)) $ zip (reverse ixs) irys
    lowerWeightedSum = reverse . scanl1 (+) . map (uncurry(+)) $ zip (reverse iys) irxs
    go !res !t !acc !s (u:us) (l:ls) ((x, y):xys) =
        go (res `max` res') (t + 2) acc' (s + x + y) us ls xys
      where
        acc'
            | rem t 4 == 0 = acc + t * x + (t + 1) * y
            | otherwise = acc + t * y + (t + 1) * x
        res'
            | rem t 4 == 0 = acc + u + (totalSum - s) * div (t + 1) 2
            | otherwise = acc + l + (totalSum - s) * div (t + 1) 2
    go !res _ acc _ _ _ [] = max acc res
