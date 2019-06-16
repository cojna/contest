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
import qualified Data.ByteString.Builder     as B
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
import qualified System.IO                   as IO
import           Unsafe.Coerce

main :: IO ()
main = do
    !n <- readLn :: IO Int
    xs <- U.unfoldrN n (C.readInt.C.dropWhile isSpace) <$> C.getLine
    let (res, qs) = solve n xs
    print $ res
    putStr$concat[shows x " " ++ shows y "\n"|(x,y)<-qs]

solve :: Int -> U.Vector Int -> (Int, [(Int, Int)])
solve n (L.sort.U.toList -> xs) = go [] pos $ reverse neg
  where
    (neg, pos) = span (<0) xs
    go [] [x0,x1] [] = (x1-x0,[(x1,x0)])
    go [] [] [y0,y1] = (y0-y1,[(y0,y1)])
    go res [x] [y] = (x-y, reverse $ (x,y):res)
    go res (x0:x1:xs) (y:ys) | !y'<-y-x0 = go ((y,x0):res) (x1:xs) (y':ys)
    go res [x] (y0:y1:ys) | !x'<-x-y0 = go ((x,y0):res) [x'] (y1:ys)
    go res (x0:x1:xs) [] | !y'<-x0-x1 = go ((x0,x1):res) xs [y']
    go res [] (y0:y1:ys) | !x'<-y0-y1 = go ((y0,y1):res) [x'] ys
    go res [] [] = error "unreachable"
