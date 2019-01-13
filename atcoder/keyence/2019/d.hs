{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE MagicHash         #-}
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
import           GHC.Exts

main :: IO ()
main = do
    [n, m] <- map read.words <$> getLine
    xs <- U.unfoldrN n (B.readInt.B.dropWhile isSpace) <$> B.getLine
    ys <- U.unfoldrN m (B.readInt.B.dropWhile isSpace) <$> B.getLine
    print $ solve n m xs ys

solve :: Int -> Int -> U.Vector Int -> U.Vector Int -> IntMod
solve n m rows cols
    | member (n * m) rowMap, member (n * m) colMap = go 1 (n * m - 1) 1 1
    | otherwise = 0
  where
    !rowMap = revMap (n * m + 1) rows
    !colMap = revMap (n * m + 1) cols

    member :: Int -> U.Vector Int -> Bool
    member x v = not . isNothing $ v U.! x

    go res 0 _  _ = res
    go res x !numR !numC
        | member x rowMap, member x colMap = go res (x - 1) (numR + 1) (numC + 1)
        | member x rowMap = go (res *% numC) (x - 1) (numR + 1) numC
        | member x colMap = go (res *% numR) (x - 1) numR (numC + 1)
        | used < numR * numC = go (res *% (numR *% numC -% used)) (x - 1) numR numC
        | otherwise = 0
      where
        !used = n * m - x


nothing :: Int
nothing = -1

isNothing :: Int -> Bool
isNothing = (== nothing)

revMap :: Int -> U.Vector Int -> U.Vector Int
revMap size v = U.accumulate (flip const) (U.replicate size nothing)
    $ U.imap (flip (,)) v

#define MOD 1000000007

modulus :: Int
modulus = MOD

infixr 8 ^%
infixl 7 *%, /%
infixl 6 +%, -%

type IntMod = Int

intMod :: Int -> IntMod
intMod x = mod x MOD

intModValidate :: Int -> Bool
intModValidate x = 0 <= x && x < MOD

(+%) :: IntMod -> IntMod -> IntMod
(I# x#) +% (I# y#) = I# ((x# +# y#) `remInt#` MOD#)
{-# INLINE (+%) #-}

(-%) :: IntMod -> IntMod -> IntMod
(I# x#) -% (I# y#) = I# ((x# -# y# +# MOD#) `remInt#` MOD#)
{-# INLINE (-%) #-}

(*%) :: IntMod -> IntMod -> IntMod
(I# x#) *% (I# y#) = I# ((x# *# y#) `remInt#` MOD#)
{-# INLINE (*%) #-}

(/%) :: IntMod -> IntMod -> IntMod
(I# x#) /% (I# y#) = go# y# MOD# 1# 0#
  where
    go# a# b# u# v#
        | isTrue# (b# ># 0#) = case a# `quotInt#` b# of
            q# -> go# b# (a# -# (q# *# b#)) v# (u# -# (q# *# v#))
        | otherwise = I# ((x# *# (u# +# MOD#)) `remInt#` MOD#)
{-# INLINE (/%) #-}

(^%) :: IntMod -> Int -> IntMod
x ^% n
    | n > 0 = go 1 x n
    | n == 0 = 1
    | otherwise = go 1 (1 /% x) (-n)
  where
    go !acc !y !m
        | m .&. 1 == 0 = go acc (y *% y) (unsafeShiftR m 1)
        | m == 1 = acc *% y
        | otherwise = go (acc *% y) (y *% y) (unsafeShiftR (m - 1) 1)