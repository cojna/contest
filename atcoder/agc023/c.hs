{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UnboxedTuples         #-}

import           Control.Applicative
import           Control.DeepSeq
import           Control.Exception
import           Control.Monad
import           Control.Monad.ST
import           Control.Monad.Trans.State.Strict
import           Data.Array.Base
import           Data.Array.ST                    (STUArray, runSTUArray)
import           Data.Bits
import           Data.Bool
import qualified Data.ByteString.Char8            as B
import           Data.Char
import           Data.Coerce
import qualified Data.Foldable                    as F
import           Data.Function
import           Data.Int
import qualified Data.IntMap.Strict               as IM
import qualified Data.IntSet                      as IS
import qualified Data.List                        as L
import qualified Data.Map.Strict                  as M
import           Data.Monoid
import           Data.Ord
import qualified Data.Set                         as S
import           Data.STRef
import           Data.Tuple
import           Data.Word
import           Debug.Trace
import           GHC.Arr                          (Array, Ix (..), STArray)
import           GHC.Exts
import           System.Exit
import           System.IO


main :: IO ()
main = do
    !n <- readLn :: IO Int
    print $ solve n

lim :: Int
lim = 1000000

solve :: Int -> Int
solve n = F.foldl' (+%) 0 $ zipWith3 (\i x y -> (x -% y) *% i) is fs $ 0:fs
  where
    is = [div (n + 1) 2 .. n - 1]
    fs = map f is
    f k = fact k *% fact (k - 1) /% fact (2 * k - n)

#define MOD 1000000007

infixl 7 *%, /%
infixl 6 +%, -%

type IntMod = Int

(+%), (-%), (*%), (/%) :: IntMod -> IntMod -> IntMod
(I# x#) +% (I# y#) = I# ((x# +# y#) `remInt#` MOD#)
(I# x#) -% (I# y#) = I# ((x# -# y# +# MOD#) `remInt#` MOD#)
(I# x#) *% (I# y#) = I# ((x# *# y#) `remInt#` MOD#)
(I# x#) /% (I# y#) = go# y# MOD# 1# 0#
  where
    go# a# b# u# v#
        | isTrue# (b# ># 0#) = case a# `quotInt#` b# of
            q# -> go# b# (a# -# (q# *# b#)) v# (u# -# (q# *# v#))
        | otherwise = I# ((x# *# (u# +# MOD#)) `remInt#` MOD#)
{-# INLINE (+%) #-}
{-# INLINE (-%) #-}
{-# INLINE (*%) #-}
{-# INLINE (/%) #-}

factCache :: UArray Int Int
factCache = runSTUArray $ do
    fact <- newArray (0, lim) 1
    F.for_ [2..lim] $ \i ->
        unsafeRead fact (i - 1) >>= unsafeWrite fact i . (*% i)
    return fact

fact :: Int -> Int
fact = unsafeAt factCache

