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
    [n, c] <- map read.words <$> getLine :: IO [Int]
    xs <- U.unfoldrN n (B.readInt.B.dropWhile isSpace) <$> B.getContents
    print $ solve c $ U.toList xs

solve :: Int -> [Int] -> Int
solve c xs = go 0 $ S.fromList $ zip xs [0..]
  where
    go !res !s = case S.maxView s of
        Just ((mm, _), rest) -> case S.minView rest of
            Just ((m, _), rest')
              | m + mm + 1 <= c -> go (res + 1) rest'
            _ -> go (res + 1) rest
        Nothing -> res
