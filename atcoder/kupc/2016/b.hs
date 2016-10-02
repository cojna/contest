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
    [n, k] <- map read.words <$> getLine :: IO [Int]
    css <- S.toList.S.fromList <$> replicateM n getLine
    print $ solve k $ map head $ sort css

solve :: Int -> [Char] -> Int
solve k cs = go 0 $ map length $ group cs
  where
    go !res xs
      | Just xs' <- step xs = go (res+1) xs'
      | otherwise = res

    step :: [Int] -> Maybe [Int]
    step xs
      | length xs >= k = Just $ sortBy (flip compare) $ filter (>0) $ zipWith (-) xs $ replicate k 1 ++ repeat 0
      | otherwise = Nothing

