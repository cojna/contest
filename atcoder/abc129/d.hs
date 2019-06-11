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
solve h w mat = subtract 1 . U.maximum $ U.zipWith (+) lr ud
  where
    trans :: U.Vector Int -> U.Vector Int
    trans m = U.unsafeBackpermute m $ U.generate (h * w) $ \ij ->
        let (i, j) = quotRem ij w
        in j * h + i

    lr :: U.Vector Int
    lr = U.concatMap (\i ->
          U.scanl1' (\acc x -> if x == 0 then 0 else max acc x)
            . U.postscanr' (\c acc -> if c /= B.c2w '.' then 0 else acc + 1) 0
            $ U.unsafeSlice (i * w) w mat
        )
        $ U.generate h id

    ud :: U.Vector Int
    ud = trans
        . U.concatMap (\j ->
            U.scanl1' (\acc x -> if x == 0 then 0 else max acc x)
              . U.postscanr' (\c acc -> if c /= B.c2w '.' then 0 else acc + 1) 0
              $ U.map (U.unsafeIndex mat) $ U.iterateN h (+w) j
        )
        $ U.generate w id
