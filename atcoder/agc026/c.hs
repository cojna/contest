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
import qualified Data.ByteString             as B
import qualified Data.ByteString.Unsafe      as B
import           Data.Char
import           Data.Function
import qualified Data.HashMap.Strict         as M
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
    !n <- readLn :: IO Int
    bs <- B.getLine
    print $ solve n bs

solve :: Int -> B.ByteString -> Int
solve n bs = sum [M.lookupDefault 0 pair rightFreq| pair<-lefts]
  where
    (left, right) = B.splitAt n bs
    lefts = pairs n left
    rights = pairs n $ B.reverse right
    !rightFreq = foldl' (\m pair -> M.insertWith (+) pair 1 m) M.empty rights

pairs n bs = [divide i bs|i<-[0..2^n-1]]
  where
    divide i bs = B.pack $ map (B.unsafeIndex bs) xis ++ 0:map (B.unsafeIndex bs) yis
      where
        (xis, yis) = partition (testBit (i :: Int)) [0..n-1]
