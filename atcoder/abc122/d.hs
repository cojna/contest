{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}

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
    !n <- readLn :: IO Int
    print $ solve n

solve :: Int -> Int
solve n = runST $ do
    dp <- UM.replicate (128 * 4 * 4 * 4) 0
    rep 4 $ \i -> do
        rep 4 $ \j -> do
            rep 4 $ \k -> do
                UM.write dp (ix 3 i j k) 1
    UM.write dp (ix 3 a c g) 0
    UM.write dp (ix 3 a g c) 0
    UM.write dp (ix 3 g a c) 0

    rep (n-3) $ \tmp -> do
        let i = tmp + 4
        -- a
        rep 4 $ \x -> do
            rep 4 $ \y -> do
                rep 4 $ \z -> do
                    prev <- UM.read dp (ix (i - 1) x y z)
                    UM.modify dp (prev +%) (ix i y z a)
        -- c
        rep 4 $ \x -> do
            rep 4 $ \y -> do
                rep 4 $ \z -> do
                    let cond = or
                            [ [y, z] == [a, g]
                            , [y, z] == [g, a]
                            , [x, y, z] == [a, g, t]
                            , [x, y, z] == [a, g, g]
                            , [x, y, z] == [a, t, g]
                            ]
                    unless cond $ do
                        prev <- UM.read dp (ix (i - 1) x y z)
                        UM.modify dp (prev +%) (ix i y z c)
        -- g
        rep 4 $ \x -> do
            rep 4 $ \y -> do
                rep 4 $ \z -> do
                    unless ([y, z] == [a, c]) $ do
                        prev <- UM.read dp (ix (i - 1) x y z)
                        UM.modify dp (prev +%) (ix i y z g)
        -- t
        rep 4 $ \x -> do
            rep 4 $ \y -> do
                rep 4 $ \z -> do
                    prev <- UM.read dp (ix (i - 1) x y z)
                    UM.modify dp (prev +%) (ix i y z t)

    rep 4 $ \x -> do
        rep 4 $ \y -> do
            rep 4 $ \z -> do
                prev <- UM.read dp (ix n x y z)
                UM.modify dp (prev +%) (ix 127 0 0 0)
    UM.read dp (ix 127 0 0 0)

a = 0
c = 1
g = 2
t = 3


ix :: Int -> Int -> Int -> Int -> Int
ix i x y z = unsafeShiftL i 6 .|. unsafeShiftL x 4 .|. unsafeShiftL y 2 .|. z

rep :: (Monad m) => Int -> (Int -> m ()) -> m ()
rep n = U.forM_ (U.generate n id)

-------------------------------------------------------------------------------

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
(I# x#) +% (I# y#) = case x# +# y# of
    r# -> I# (r# -# ((r# >=# MOD#) *# MOD#))
{-# INLINE (+%) #-}

(-%) :: IntMod -> IntMod -> IntMod
(I# x#) -% (I# y#) = case x# -# y# of
    r# -> I# (r# +# ((r# <# 0#) *# MOD#))
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
