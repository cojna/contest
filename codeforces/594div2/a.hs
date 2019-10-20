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

main :: IO ()
main = do
  t <- readLn
  replicateM_ t $ do
      n <- readLn
      ps <- L.unfoldr (C.readInt.C.dropWhile isSpace) <$> C.getLine
      m <- readLn
      qs <- L.unfoldr (C.readInt.C.dropWhile isSpace) <$> C.getLine
      print $ solve n ps m qs
      IO.hFlush IO.stdout

solve :: Int -> [Int] -> Int -> [Int] -> Int64
solve n xs m ys = fromIntegral xsEven * fromIntegral ysEven + fromIntegral xsOdd * fromIntegral ysOdd
  where
    p x = x .&. 1 == 0
    !xsEven = length $ filter p xs
    !xsOdd = n - xsEven
    !ysEven = length $ filter p ys
    !ysOdd = m - ysEven
