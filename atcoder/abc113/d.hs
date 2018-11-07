{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}
{-# LANGUAGE MagicHash    #-}
{-# LANGUAGE MultiWayIf   #-}

import           Data.Bits
import qualified Data.Vector.Unboxed as U
import           Debug.Trace
import           GHC.Exts

#define MOD 1000000007

main :: IO ()
main = do
    [h, w, k] <- map read.words <$> getLine :: IO [Int]
    print $ solve h w k

solve :: Int -> Int -> Int -> IntMod
solve _ 1 _ = 1
solve h w k = dph U.! (k - 1)
  where
    dp0 = U.fromList . take w $ 1 : repeat 0
    dph = iterate step dp0 !! h
    step :: U.Vector IntMod -> U.Vector IntMod
    step !dp = U.generate w $ \i ->
      if | i == 0 -> dp U.! i *% (table U.! (w - 1))
            +% dp U.! (i + 1) *% (table U.! (w - 2))
         | i == w - 1 -> dp U.! (i - 1) *% (table U.! (w - 2))
            +% dp U.! i *% (table U.! (w - 1))
         | otherwise -> dp U.! (i - 1) *% (table U.! (i - 1)) *% (table U.! (w - i - 1))
            +% dp U.! i *% (table U.! i) *% (table U.! (w - i - 1))
            +% dp U.! (i + 1) *% (table U.! i) *% (table U.! (w - i - 2))

table :: U.Vector IntMod
table = U.fromList . take 10 $ 1 : 1 : mkTable 1 0

mkTable !w !b = (2 *% w +% b) : mkTable (w +% b) w

modulus :: Int
modulus = MOD

infixr 8 ^%
infixl 7 *%, /%
infixl 6 +%, -%

type IntMod = Int

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
