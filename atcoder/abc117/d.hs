{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}

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
import qualified Data.Foldable               as F
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
    [n, k] <- map read.words <$> getLine :: IO [Int]
    xs <- U.unfoldrN n (B.readInt.B.dropWhile isSpace) <$> B.getLine
    print $ solve n k xs

solve :: Int -> Int -> U.Vector Int -> Int
solve n k xs = F.foldl' max (f k) [f $ g i | i<-[0..63], testBit k i]
  where
    f x = U.sum $ U.map (xor x) xs
    g i = F.foldl' (.|.) (k .&. complement (bit (i + 1) - 1))
        [bool 0 (bit j) $ 2 * (freq U.! j) < n | j<-[0..i-1]]

    !freq = U.generate 64 $ \i ->
        U.length $ U.filter (flip testBit i) xs
