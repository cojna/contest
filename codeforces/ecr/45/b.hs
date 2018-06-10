{-# OPTIONS_GHC -O2 -funbox-strict-fields #-}
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
    [n,k] <- map read.words <$> getLine
    xs <- unfoldr (B.readInt.B.dropWhile isSpace) <$> B.getLine
    print $ solve k xs

solve :: Int -> [Int] -> Int
solve k0 xs0 = go 0 . group . map fromIntegral $ sort xs0
  where
    k :: Int64
    !k = fromIntegral k0
    go !res (xs@(x:_):ys@(y:_):xss)
      | y <= x + k = go res (ys:xss)
      | otherwise = go (res + length xs) (ys:xss)
    go res xs = res + length (concat xs)
