{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}
#ifndef DEBUG
{-# LANGUAGE Safe              #-}
#endif

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.State.Strict
import qualified Data.Array                 as A
import qualified Data.Array.Unboxed         as UA
import qualified Data.Array.ST.Safe         as MA
import           Data.Bool
import qualified Data.ByteString.Builder    as B
import qualified Data.ByteString.Char8      as C
import           Data.Char
import           Data.Function
import qualified Data.IntMap.Strict         as IM
import qualified Data.IntSet                as IS
import qualified Data.List                  as L
import qualified Data.List.NonEmpty         as NL
import           Data.Monoid
import qualified Data.Map.Strict            as M
import           Data.Monoid
import           Data.Ord
import           Data.Semigroup
import qualified Data.Set                   as S
import           Data.Tuple
import           Foreign
import qualified System.IO                  as IO
#ifdef DEBUG
import           Debug.Trace
#endif

#define MOD 1000000007

main :: IO ()
main = do
  [n, m] <- map read.words <$> getLine
  print $ solve n m

solve :: Integer -> Integer -> Integer
solve n m = powMod (powMod 2 m MOD - 1) n MOD

powMod :: (Integral a, Integral b, Bits b) => a -> b -> a -> a
powMod x n m
    | n > 0 = go 1 x n
    | n == 0 = 1
    | otherwise = undefined
  where
    go !acc !y !i
        | i .&. 1 == 0 = go acc (y * y `rem` m) (unsafeShiftR i 1)
        | i == 1 = acc * y `rem` m
        | otherwise = go (acc * y `rem` m) (y * y `rem` m) (unsafeShiftR (i - 1) 1)
{-# INLINE powMod #-}
