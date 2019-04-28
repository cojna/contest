{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Control.Monad.State.Strict
import           Data.Bool
import qualified Data.ByteString             as B
import qualified Data.ByteString.Char8       as C
import qualified Data.ByteString.Unsafe      as B
import           Data.Char
import qualified Data.Foldable               as F
import           Data.Function
import qualified Data.IntMap.Strict          as IM
import qualified Data.IntSet                 as IS
import qualified Data.List                   as L
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
    [h, w] <- map read.words <$> getLine :: IO [Int]
    m <- U.unfoldrN (h * w) (C.readInt.C.dropWhile isSpace) <$> C.getContents
    let moves = solve h w m
    print $ length moves
    putStr.unlines $ map show moves

data Move = Move !Int !Int !Int !Int

instance Show Move where
    show (Move y x y' x') = unwords $ map (show.succ) [y, x, y', x']

solve :: Int -> Int -> U.Vector Int -> [Move]
solve h w m = go (-1, -1) False . U.toList . U.generate (h * w) $ \xy ->
    case unIx xy of
        (x, y)
            | even x -> (x, y)
            | otherwise -> (x, w - 1 - y)
  where
    go (!prevX, !prevY) !hasCarry ((x, y):xys)
        | hasCarry
            = Move prevX prevY x y : go (x, y) (not isOdd) xys
        | otherwise = go (x, y) isOdd xys
      where
        isOdd = testBit (U.unsafeIndex m (ix x y)) 0
    go _ _ [] = []

    ix x y = x * w + y
    unIx xy = quotRem xy w
