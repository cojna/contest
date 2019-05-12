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
    !n <- readLn :: IO Int
    xs <- U.unfoldrN n (C.readInt.C.dropWhile isSpace) <$> C.getLine
    print $ solve n xs

nothing :: Int
nothing = -1

isNothing :: Int -> Bool
isNothing = (<0)

solve :: Int -> U.Vector Int -> Int
solve n xs
    | U.last xors /= 0 = dp U.! U.last xors
    | otherwise = U.foldl' (+%) 0 dp
  where
    !dp = U.create $ do
        dp0 <- UM.replicate (shiftL 1 20) (1 :: Int)
        dpx <- UM.replicate (shiftL 1 20) (0 :: Int)
        prevPos <- UM.replicate (shiftL 1 20) nothing
        flip U.imapM xors $ \i -> \case
            0 -> do
                prev <- UM.unsafeRead prevPos 0
                UM.unsafeWrite prevPos 0 i
                if isNothing prev
                then UM.unsafeWrite dpx 0 1
                else UM.unsafeModify dpx (*% 2) 0
            x -> do
                prev <- UM.unsafeRead prevPos x
                UM.unsafeWrite prevPos x i
                if isNothing prev
                then UM.unsafeWrite dpx x 1
                else do
                    let !z = zero prev i
                    f0 <- UM.unsafeRead dp0 x
                    fx <- UM.unsafeRead dpx x
                    UM.unsafeWrite dp0 x $ f0 +% z *% fx
                    UM.unsafeWrite dpx x $ f0 +% (1 + z) *% fx
        return $ bool dp0 dpx $ U.last xors == 0
    !xors = U.postscanl' xor 0 xs
    !zeros = U.scanl' (+) 0 $ U.map (bool 0 1 . (== 0)) xors
    -- [l..r)
    zero l r
        | l < r = zeros U.! r - zeros U.! l
        | otherwise = 0


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