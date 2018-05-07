{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe              #-}
{-# LANGUAGE TupleSections     #-}

import           Control.Applicative
import           Control.Monad
import           Data.Array.Unboxed
import           Data.Bits
import           Data.Bool
import qualified Data.ByteString.Char8 as B
import           Data.Char
import           Data.Function
import           Data.Int
import qualified Data.IntMap.Strict    as IM
import qualified Data.IntSet           as IS
import           Data.List
import qualified Data.Map.Strict       as M
import           Data.Monoid
import           Data.Ord
import qualified Data.Set              as S
import           Data.Tuple
import           Data.Word

main :: IO ()
main = do
    [n, k] <- map read.words <$> getLine
    xs <- unfoldr (B.readInt.B.dropWhile isSpace) <$> B.getLine
    print . maybe (-1) id $ solve n k xs

solve :: Int -> Int -> [Int] -> Maybe Int
solve _ 0 xs
  | elem 1 xs = Nothing
  | otherwise = Just 1
solve _ k xs
    | k == length (takeWhile (<= pivot) sorted) = Just pivot
    | otherwise = Nothing
  where
    !sorted = sort xs
    !pivot = sorted !! (k - 1)
