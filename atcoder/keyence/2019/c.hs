{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Control.Monad.State.Strict
import           Data.Bool
import qualified Data.ByteString.Char8       as B
import qualified Data.ByteString.Unsafe      as B
import           Data.Char
import           Data.Function
import qualified Data.IntMap.Strict          as IM
import qualified Data.IntSet                 as IS
import           Data.List
import           Data.Monoid
import           Data.Ord
import           Data.Primitive.MutVar
import qualified Data.Set                    as S
import           Data.Tuple
import qualified Data.Vector                 as V
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM
import           Debug.Trace
import           Foreign                     hiding (void)
import           Unsafe.Coerce

main :: IO ()
main = do
    n <- readLn
    xs <- U.unfoldrN n (B.readInt.B.dropWhile isSpace) <$> B.getLine
    ys <- U.unfoldrN n (B.readInt.B.dropWhile isSpace) <$> B.getLine
    print . maybe (-1) id $ solve n xs ys

solve :: Int -> U.Vector Int -> U.Vector Int -> Maybe Int
solve n xs ys
    | sx < sy = Nothing
    | otherwise = Just . go res0 need0 . map negate . sort $ U.toList negatives
  where
    !sx = U.sum xs
    !sy = U.sum ys
    !diffs = U.filter (/= 0) $ U.zipWith (-) ys xs
    (positives, negatives) = U.partition (> 0) diffs
    !res0 = U.length positives
    !need0 = U.sum positives

    go !res !need (x:xs)
      | need <= 0 = res
      | otherwise = go (res + 1) (need - x) xs
    go res _ _ = res