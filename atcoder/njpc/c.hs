{-# OPTIONS_GHC -O2 -funbox-strict-fields #-}
{-# LANGUAGE BangPatterns #-}

import           Control.Applicative
import           Data.Bool
import qualified Data.ByteString.Char8 as B
import           Data.Char
import qualified Data.Vector.Unboxed   as U

main :: IO ()
main = do
    [n,l] <- map read.words <$> getLine :: IO [Int]
    xs <- U.unfoldrN n (B.readInt.B.dropWhile isSpace) <$> B.getContents
    putStrLn.bool"NO""YES"$solve l xs

solve :: Int -> U.Vector Int -> Bool
solve l xs = go 1 $ U.toList xs
 where
   go !cur (x:xs)
     | x < cur = False
     | otherwise = go next zs
     where
       next = max (cur + 2 * l) (z + l)
       z = last $ x:ys
       (ys, zs) = span (< (x + l)) xs
   go _ _ = True

