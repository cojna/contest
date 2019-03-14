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
import           Unsafe.Coerce

main :: IO ()
main = do
    !n <- readLn :: IO Int
    xys <- U.unfoldrN (div n 2) parseInt2 <$> B.getLine
    print $ solve n xys

lim :: Int
lim = 10^5

solve :: Int -> U.Vector (Int, Int) -> Int
solve 2 xys
    | uncurry (/=) $ U.head xys = 0
    | otherwise = 1
solve n xys
    | freqMaxIxX /= freqMaxIxY = n - U.maximum freqX - U.maximum freqY
    | otherwise = minimum
        [ n - freqX U.! freqMaxIxX - freqMaxY'
        , n - freqMaxX' - freqY U.! freqMaxIxY
        ]
  where
    xs = U.map fst xys
    ys = U.map snd xys

    freqX = freq xs
    freqY = freq ys

    freqMaxIxX = U.maxIndex freqX
    freqMaxIxY = U.maxIndex freqY

    freqMaxX' = U.maximum $ U.ifilter (\i _ -> i /= freqMaxIxX) freqX
    freqMaxY' = U.maximum $ U.ifilter (\i _ -> i /= freqMaxIxY) freqY

freq :: U.Vector Int -> U.Vector Int
freq = U.accumulate (+) (U.replicate (lim + 1) 0) . U.map (flip (,) 1)

-------------------------------------------------------------------------------
type Parser a = B.ByteString -> Maybe (a, B.ByteString)

parseInt :: Parser Int
parseInt = B.readInt . B.dropWhile isSpace

parseInt2 :: Parser (Int, Int)
parseInt2 = runStateT $
    (,) <$> StateT (B.readInt . B.dropWhile isSpace)
        <*> StateT (B.readInt . B.unsafeTail)

parseInt3 :: Parser (Int, Int, Int)
parseInt3 = runStateT $
    (,,) <$> StateT (B.readInt . B.dropWhile isSpace)
        <*> StateT (B.readInt . B.unsafeTail)
        <*> StateT (B.readInt . B.unsafeTail)

parseInt4 :: Parser (Int, Int, Int, Int)
parseInt4 = runStateT $
    (,,,) <$> StateT (B.readInt . B.dropWhile isSpace)
        <*> StateT (B.readInt . B.unsafeTail)
        <*> StateT (B.readInt . B.unsafeTail)
        <*> StateT (B.readInt . B.unsafeTail)