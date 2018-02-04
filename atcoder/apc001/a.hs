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
    [x, y] <- map read.words <$> getLine :: IO [Integer]
    print $ solve x y

solve :: Integer -> Integer -> Integer
solve a b
  | mod a b == 0 = -1
  | otherwise = head.filter (\x -> rem x b /= 0 && x <= 1000000000000000000)$ map ((a*).toInteger) xor128

-------------------------------------------------------------------------------

xor128 :: [Word32]
xor128 = go 123456789 362436069 521288629 88675123
  where
    go !x !y !z !w = neww : go y z w neww
      where
        !t = x `xor` unsafeShiftL x 11
        !neww = (w `xor` unsafeShiftR w 19) `xor` (t `xor` unsafeShiftR t 8)
