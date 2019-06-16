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
import qualified Data.ByteString.Internal    as B
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
import qualified Data.Vector.Storable         as U
import qualified Data.Vector.Unboxed.Mutable as UM
import           Debug.Trace
import           Foreign                     hiding (void)
import           GHC.Exts
import qualified System.IO                   as IO
import           Unsafe.Coerce

main :: IO ()
main = do
    [h, w] <- map read.words <$> getLine :: IO [Int]
    B.PS p 0 l <- C.hGetNonBlocking IO.stdin (h*(w+1))
    let mat = U.unsafeFromForeignPtr0 p l
    print $ solve h (w+1) mat

solve :: Int -> Int -> U.Vector Word8 -> Int
solve h w mat = subtract 1 . V.maximum . V.map U.maximum $ V.zipWith (U.zipWith (+)) lr ud
  where
    forward :: Int -> Int -> Int
    forward acc x
        | x == 0 = 0
        | otherwise = max acc x

    backward :: Word8 -> Int -> Int
    backward x acc
        | x /= B.c2w '.' = 0
        | otherwise = acc + 1

    lr :: V.Vector (U.Vector Int)
    lr  = V.generate h $ \i ->
          U.scanl1' forward
            . U.postscanr' backward 0
            $ U.unsafeSlice (i * w) w mat

    ud :: V.Vector (U.Vector Int)
    ud = V.scanl1' (U.zipWith forward)
        . V.postscanr' (U.zipWith backward) (U.replicate w 0)
        $ V.generate h (\i -> U.unsafeSlice (i * w) w mat)


