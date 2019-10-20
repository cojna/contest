{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}
#ifndef DEBUG
{-# LANGUAGE Safe              #-}
#endif

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.State.Strict
import qualified Data.Array                 as A
import qualified Data.Array.Unboxed         as UA
import qualified Data.Array.ST.Safe         as MA
import           Data.Bool
import qualified Data.ByteString.Builder    as B
import qualified Data.ByteString.Char8      as C
import           Data.Char
import           Data.Function
import qualified Data.IntMap.Strict         as IM
import qualified Data.IntSet                as IS
import qualified Data.List                  as L
import qualified Data.List.NonEmpty         as NL
import           Data.Monoid
import qualified Data.Map.Strict            as M
import           Data.Monoid
import           Data.Ord
import           Data.Ratio
import           Data.Semigroup
import qualified Data.Set                   as S
import           Data.Tuple
import           Foreign
import qualified System.IO                  as IO
#ifdef DEBUG
import           Debug.Trace
#endif

#define MOD 1000000007

main :: IO ()
main = do
  [n, m] <- map read.words <$> getLine
  print $ solve n m

solve :: Int -> Int -> IntMod
solve n m | m < n = solve m n
solve 1 1 = 2
solve n m = resM * 2 - 2 + resN * 2
  where
    !resN = last . take n $ go 1 2
    !resM = last . take m $ go 1 2
    go !x !y = x : go y z
      where
        !z = x + y
-------------------------------------------------------------------------------
-- Data.IntMod.Safe
-------------------------------------------------------------------------------

modulus :: (Num a) => a
modulus = MOD
{-# INLINE modulus #-}

infixr 8 ^%
infixl 7 *%, /%
infixl 6 +%, -%

(+%) :: Int64 -> Int64 -> Int64
x +% y = case x + y of
    r | r < MOD -> r
      | otherwise -> r - MOD
{-# INLINE (+%) #-}

(-%) :: Int64 -> Int64 -> Int64
x -% y = case x - y of
    r | r < 0 -> r + MOD
      | otherwise -> r
{-# INLINE (-%) #-}

(*%) :: Int64 -> Int64 -> Int64
x *% y = x * y `rem` MOD
{-# INLINE (*%) #-}

-- |
-- >>> 1 /% 0
-- 0
(/%) :: Int64 -> Int64 -> Int64
x /% y = go y MOD 1 0
  where
    go !a !b !u !v
        | b > 0 = case a `quot` b of
            q -> go b (a - q * b) v (u - q * v)
        | otherwise = x * (u + MOD) `rem` MOD
{-# INLINE (/%) #-}

(^%) :: Int64 -> Int -> Int64
x ^% n
    | n > 0 = go 1 x n
    | n == 0 = 1
    | otherwise = go 1 (1 /% x) (-n)
  where
    go !acc !y !m
        | m .&. 1 == 0 = go acc (y *% y) (unsafeShiftR m 1)
        | m == 1 = acc *% y
        | otherwise = go (acc *% y) (y *% y) (unsafeShiftR (m - 1) 1)

newtype IntMod = IntMod{getIntMod :: Int64} deriving (Eq, Ord)

intMod :: (Integral a) => a -> IntMod
intMod x = fromIntegral $ mod (fromIntegral x) MOD
{-# INLINE intMod #-}

intModValidate :: IntMod -> Bool
intModValidate (IntMod x) = 0 <= x && x < MOD
{-# INLINE intModValidate #-}

instance Show IntMod where
    show (IntMod x) = show x

instance Bounded IntMod where
    minBound = IntMod 0
    maxBound = IntMod $ MOD - 1

instance Enum IntMod where
    toEnum = intMod
    fromEnum = fromIntegral

instance Real IntMod where
    toRational (IntMod x) = toRational x

instance Integral IntMod where
    quotRem x y = (x / y, x - x / y * y)
    toInteger (IntMod x) = toInteger x

instance Num IntMod where
    (IntMod x) + (IntMod y) = IntMod (x +% y)
    (IntMod x) - (IntMod y) = IntMod (x -% y)
    (IntMod x) * (IntMod y) = IntMod (x *% y)
    abs = id
    signum = const (IntMod 1)
    fromInteger x = IntMod . fromInteger $ mod x modulus

instance Fractional IntMod where
    (IntMod x) / (IntMod y) = IntMod (x /% y)
    fromRational q = fromInteger (numerator q) / fromInteger (denominator q)
