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
    [h, w, n] <- map read.words <$> getLine :: IO [Int]
    [sr, sc] <- map read.words <$> getLine :: IO [Int]
    s <- getLine
    t <- getLine
    putStrLn.bool"NO""YES"$solve h w sr sc s t

solve :: Int -> Int -> Int -> Int -> String -> String -> Bool
solve h w sr sc s t = go0 1 h 1 w (reverse s) . tail $ reverse t
  where
    validate rMin rMax cMin cMax = rMin <= rMax && cMin <= cMax
    go0 !rMin !rMax !cMin !cMax (x:xs) ys
        | validate rMin rMax cMin cMax = case x of
            'L' -> go1 rMin rMax (cMin + 1) cMax xs ys
            'R' -> go1 rMin rMax cMin (cMax - 1) xs ys
            'U' -> go1 (rMin + 1) rMax cMin cMax xs ys
            'D' -> go1 rMin (rMax - 1) cMin cMax xs ys
        | otherwise = False
    go0 rMin rMax cMin cMax [] []
        = rMin <= sr && sr <= rMax && cMin <= sc && sc <= cMax
    go1 rMin rMax cMin cMax xs (y:ys)
        | validate rMin rMax cMin cMax = case y of
            'L' -> go0 rMin rMax cMin (min w $ cMax + 1) xs ys
            'R' -> go0 rMin rMax (max 1 $ cMin - 1) cMax xs ys
            'U' -> go0 rMin (min h $ rMax + 1) cMin cMax xs ys
            'D' -> go0 (max 1 $ rMin - 1) rMax cMin cMax xs ys
        | otherwise = False
    go1 rMin rMax cMin cMax [] []
        = rMin <= sr && sr <= rMax && cMin <= sc && sc <= cMax

-------------------------------------------------------------------------------
type Parser a = C.ByteString -> Maybe (a, C.ByteString)

parseInt :: Parser Int
parseInt = C.readInt . C.dropWhile isSpace

parseInt2 :: Parser (Int, Int)
parseInt2 = runStateT $
    (,) <$> StateT (C.readInt . C.dropWhile isSpace)
        <*> StateT (C.readInt . B.unsafeTail)

parseInt3 :: Parser (Int, Int, Int)
parseInt3 = runStateT $
    (,,) <$> StateT (C.readInt . C.dropWhile isSpace)
        <*> StateT (C.readInt . B.unsafeTail)
        <*> StateT (C.readInt . B.unsafeTail)

parseInt4 :: Parser (Int, Int, Int, Int)
parseInt4 = runStateT $
    (,,,) <$> StateT (C.readInt . C.dropWhile isSpace)
        <*> StateT (C.readInt . B.unsafeTail)
        <*> StateT (C.readInt . B.unsafeTail)
        <*> StateT (C.readInt . B.unsafeTail)