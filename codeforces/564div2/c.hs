{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
#ifndef DEBUG
{-# LANGUAGE Safe              #-}
#endif

import           Control.Applicative
import           Control.Monad
import           Control.Monad.State.Strict
import           Data.Array.Unboxed
import           Data.Bits
import           Data.Bool
import qualified Data.ByteString.Char8      as C
import           Data.Char
import           Data.Function
import           Data.Int
import qualified Data.IntMap.Strict         as IM
import qualified Data.IntSet                as IS
import qualified Data.List                  as L
import qualified Data.Map.Strict            as M
import           Data.Monoid
import           Data.Ord
import qualified Data.Set                   as S
import           Data.Tuple
import           Data.Word
#ifdef DEBUG
import           Debug.Trace
#endif
import Foreign

main :: IO ()
main = do
  !n <- readLn :: IO Int
  xs <- L.unfoldr (C.readInt.C.dropWhile isSpace) <$> C.getLine
  ys <- L.unfoldr (C.readInt.C.dropWhile isSpace) <$> C.getLine
  print $ solve n xs ys

solve :: Int -> [Int] -> [Int] -> Int
solve n xs ys
  | Just res <- solveNon1 n xs ys = res
  | otherwise = n + lowerBound 0 n simulate
  where
    xs' = filter (/= 0) xs
    simulate i = go 1 (IS.fromList $ xs' ++ filter (/= 0) ys0) ys1
      where
        (ys0, ys1) = splitAt i ys
        go !acc !set (z:zs)
          | acc > n = True
          | IS.member acc set = go (acc+1) (IS.insert z set) zs
          | otherwise = False
        go _ _ [] = True

solveNon1 :: Int -> [Int] -> [Int] -> Maybe Int
solveNon1 n xs _ | elem 1 xs = Nothing
solveNon1 n xs ys
  | ys1 == [1..n-k], and (zipWith p ys0 [n-k+1..]) = Just k
  | otherwise = Nothing
 where
  (ys0, ys1) = span (/=1) ys
  !k = length ys0
  p 0 y = True
  p x y = x > y

lowerBound :: (Integral i) => i -> i -> (i -> Bool) -> i
lowerBound low high p = go low high
  where
    go !low !high
        | high <= low = high
        | p mid       = go low       mid
        | otherwise   = go (mid + 1) high
      where
        h = toInteger high
        l = toInteger low
        mid = fromIntegral $ l + div (h - l) 2
{-# INLINE lowerBound #-}
