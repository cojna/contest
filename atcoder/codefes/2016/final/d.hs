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
    [n, m] <- map read.words <$> getLine :: IO [Int]
    xs <- U.unfoldrN n (B.readInt.B.dropWhile isSpace) <$> B.getLine
    print $ solve m xs

solve :: Int -> U.Vector Int -> Int
solve m xs = go 0 $ IM.keys im
  where
    !im = U.foldl' (\acc x -> IM.insertWith (++) (rem x m) [x] acc) IM.empty xs
    go !acc (k:ks)
      | (k + k == m || k == 0), Just xs<-IM.lookup k im = go (acc + same xs) ks
      | Just xs<-IM.lookup k im = case IM.lookup (m-k) im of
          Just ys | k < (m-k) -> go (acc + diff xs ys) ks
                  | otherwise -> go acc ks
          Nothing -> go (acc + go0 0 (sort xs)) ks
    go acc [] = acc
    same xs = length xs `div` 2
    diff :: [Int] -> [Int] -> Int
    diff xs ys
        | lx == ly = lx
        | lx < ly = go0 lx $ drop lx ys'
        | otherwise = go0 ly $ drop ly xs'
      where
        !lx = length xs
        !ly = length ys
        xs' = concat . map snd . sort . concatMap f .group $ sort xs
        ys' = concat . map snd . sort . concatMap f .group $ sort ys
        f :: [Int] -> [(Int, [Int])]
        f xs
            | even l || l == 1 = [(l, xs)]
            | otherwise = [(1, [head xs]), (l-1, tail xs)]
          where
            !l = length xs
    go0 !acc (x:y:xs)
         | x == y = go0 (acc+1) xs
         | otherwise = go0 acc (y:xs)
    go0 acc _ = acc