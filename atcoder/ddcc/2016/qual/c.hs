{-# LANGUAGE BangPatterns #-}

import           Control.Applicative
import qualified Data.ByteString.Char8 as B
import           Data.Char
import qualified Data.IntMap.Strict    as IM
import qualified Data.Vector.Unboxed   as U

main :: IO ()
main = do
    [n,k] <- map read.words <$> getLine :: IO [Int]
    xs <- U.unfoldrN n (B.readInt.B.dropWhile isSpace) <$> B.getLine
    print $ solve n k xs

solve :: Int -> Int -> U.Vector Int -> Int
solve n k xs = sum . map f $ IM.keys gcds
  where
    ms = U.map (gcd k) xs
    !gcds = U.foldl' (\im g->IM.insertWith (+) g 1 im) IM.empty $ U.map (gcd k) xs

    l x = maybe 0 id $ IM.lookup x gcds
    f x = sum[l x * (l x - 1) `div` 2|rem (x * x) k == 0]
        + sum[l x * l y|y<-IM.keys gcds, x < y, rem (x * y) k == 0]
