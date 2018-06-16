{-# LANGUAGE CPP       #-}
{-# LANGUAGE MagicHash #-}

import           Control.Monad
import qualified Data.ByteString.Char8 as B
import qualified Data.List             as L
import qualified Data.Vector.Unboxed   as U
import           GHC.Exts

#define MOD 998244353

main :: IO ()
main = do
    [n,a,b,k] <- map read.words <$> getLine
    print $ solve n a b k

solve :: Int -> Int -> Int -> Int -> Int
solve n a b k = L.foldl' (+%) 0 $ do
    numA <- [0..min (div k a) n]
    (numB, 0) <- return $ quotRem (k - a * numA) b
    guard $ numB <= n
    return $! comb n numA *% comb n numB

comb :: Int -> Int -> Int
comb n k = U.unsafeIndex factCache n
    *% U.unsafeIndex recipFactCache k
    *% U.unsafeIndex recipFactCache (n - k)

lim :: Int
lim = 300000

factCache :: U.Vector Int
factCache = U.scanl' (*%) 1 $ U.generate lim (+1)

recipFactCache :: U.Vector Int
recipFactCache = U.scanr' (*%) (1 /% U.last factCache) $ U.generate lim (+1)

infixl 7 *%, /%
infixl 6 +%, -%

type IntMod = Int

intMod :: Int -> IntMod
intMod x = mod x MOD

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
