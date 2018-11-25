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
    n <- readLn :: IO Int
    cs <- U.unfoldrN n B.uncons <$> B.getLine
    q <- readLn :: IO Int
    ks <- map read.words <$> getLine
    mapM_ (print.solve cs) ks

solve :: U.Vector Char -> Int -> Int
solve cs k = fst $ U.foldl' step (U.foldl' step0 (0, emptyQ) pre) $ U.zip cs post
  where
    (pre, post) = U.splitAt k cs
    step0 (!res, !q) c
        | c == 'C' = (res + score q', q')
        | otherwise = (res, q')
      where
        q' = push c q

    step (!res, !q) (ck, c)
        | c == 'C' = (res + score q', q')
        | otherwise = (res, q')
      where
        q' = push c $ pop ck q

data Q = Q !Int !Int !Int !Int

emptyQ :: Q
emptyQ = Q 0 0 0 0

push :: Char -> Q -> Q
push 'D' (Q d m dm f) = Q (d + 1) m dm f
push 'M' (Q d m dm f) = Q d (m + 1) (dm + d) f
push c q              = q

pop :: Char -> Q -> Q
pop c (Q d m dm f) = case c of
    'D' -> Q (d - 1) m (dm - m) (f + 1)
    'M' -> Q d (m - 1) dm (f + 1)
    _   -> Q d m dm (f + 1)

score :: Q -> Int
score (Q _ _ dm _) = dm
