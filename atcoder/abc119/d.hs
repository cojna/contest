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
import qualified Data.Foldable               as F
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
    [a, b, q] <- map read.words <$> getLine :: IO [Int]
    sstsxs <- U.unfoldrN (a + b + q) (B.readInt.B.dropWhile isSpace) <$> B.getContents
    let (ss, tsxs) = U.splitAt a sstsxs
    let (ts, xs) = U.splitAt b tsxs
    putStr.unlines.map show $ solve ss ts xs

inf :: Int
inf = 0x3f3f3f3f3f3f3f3f

solve :: U.Vector Int -> U.Vector Int -> U.Vector Int -> [Int]
solve ss ts xs = map f $ U.toList xs
  where
    f x = minimum
        [ abs (sl - x) + abs (tl - sl) `min` abs (tr - sl)
        , abs (sr - x) + abs (tl - sr) `min` abs (tr - sr)
        , abs (tl - x) + abs (sl - tl) `min` abs (sr - tl)
        , abs (tr - x) + abs (sl - tr) `min` abs (sr - tr)
        ]
      where
        (!sl, !sr) = neighbor x ss
        (!tl, !tr) = neighbor x ts

neighbor :: Int -> U.Vector Int -> (Int, Int)
neighbor x vec
    | x < U.head vec = (inf, U.head vec)
    | i + 1 < U.length vec = (U.unsafeIndex vec i, U.unsafeIndex vec (i + 1))
    | otherwise = (U.last vec, inf)
  where
    i = upperBound 0 (U.length vec - 1) (\i -> U.unsafeIndex vec i <= x)

-------------------------------------------------------------------------------

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

upperBound :: (Integral i) => i -> i -> (i -> Bool) -> i
upperBound low high p
    | p high = high
    | otherwise = lowerBound low high (not.p) - 1
{-# INLINE upperBound #-}
