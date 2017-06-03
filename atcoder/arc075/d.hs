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
import           Data.List                   hiding (insert)
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
import           GHC.Exts
import           Unsafe.Coerce


main :: IO ()
main = do
    [n, a, b] <- map read.words <$> getLine :: IO [Int]
    xs <- U.unfoldrN n (B.readInt.B.dropWhile isSpace) <$> B.getContents
    print $ solve a b $ U.toList xs

solve a b xs = lowerBound 0 1000000000 predicate
  where
      !d = a - b
      predicate i = go i xs
        where
          go !rest (x:xs)
            | rest < 0 = False
            | otherwise = go (rest - y) xs
            where
              y = (d - 1 + max 0 (x - i * b)) `quot` d
          go rest [] = rest >= 0

-------------------------------------------------------------------------------

lowerBound :: (Integral i) => i -> i -> (i -> Bool) -> i
lowerBound low high p = go low high
  where
    go !low !high
        | high <= low = high
        | p mid       = go low       mid
        | otherwise   = go (mid + 1) high
      where
        mid = (low + high) `quot` 2
