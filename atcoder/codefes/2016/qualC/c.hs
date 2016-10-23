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
    print $ solve xs ys

modulus :: Int
modulus = 1000000007

solve (x:xs) ys0 = go 1 rxs rys
  where
    rxs = (x, x) : go0 x xs
    (y:ys) = reverse ys0
    rys = reverse $ (y, y) : go0 y ys
    go0 acc (z:zs)
      | acc < z = (z, z) : go0 z zs
      | otherwise = (1, z) : go0 acc zs
    go0 _ [] = []

    go !acc ((lx, ux):rxs) ((ly, uy):rys)
      | max lx ly <= min ux uy = go (acc * (min ux uy - max lx ly + 1) `mod` modulus) rxs rys
      | otherwise = 0
    go acc _ _ = acc `mod` modulus


