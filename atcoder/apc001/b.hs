{-# OPTIONS_GHC -O2 -funbox-strict-fields #-}
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
import qualified Data.Map.Strict             as M
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
    !n <- readLn :: IO Int
    xs <- U.toList.U.unfoldrN n (B.readInt.B.dropWhile isSpace) <$> B.getLine
    ys <- U.toList.U.unfoldrN n (B.readInt.B.dropWhile isSpace) <$> B.getLine
    putStrLn.bool"No""Yes" $ solve xs ys

solve :: [Int] -> [Int] -> Bool
solve xs ys = go 0 0 xs ys
  where
    go r2 r1 [x] [y] = x' <= y'
      where
        x' = x + 2 * r2
        y' = y + r1
    go r2 r1 (x:xs) (y:ys)
      | x == y = go r2 r1 xs ys
      | x < y = case quotRem (y - x) 2 of
          (_, 1) -> go r2 r1 ((x+2):xs) ((y+1):ys)
          (q, _)
            | q <= r2 -> go (r2-q) r1 xs ys
            | otherwise -> go 0 (r1 + q - r2) xs ys
      | x <= y + r1 = go r2 (r1 - (x - y)) xs ys
      | otherwise = go (r2 + x - y - r1) 0 xs ys


