{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns, BinaryLiterals, CPP, DerivingStrategies  #-}
{-# LANGUAGE DerivingVia, FlexibleContexts, FlexibleInstances       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, KindSignatures, LambdaCase #-}
{-# LANGUAGE MagicHash, MultiParamTypeClasses, MultiWayIf           #-}
{-# LANGUAGE NumericUnderscores, OverloadedStrings, PatternSynonyms #-}
{-# LANGUAGE RankNTypes, RecordWildCards, ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving, TupleSections, TypeApplications    #-}
{-# LANGUAGE TypeFamilies, TypeInType, UnboxedTuples, ViewPatterns  #-}

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.Reader
import           Control.Monad.ST
import           Control.Monad.State.Strict
import           Data.Bifunctor
import           Data.Bool
import qualified Data.ByteString               as B
import qualified Data.ByteString.Builder       as B
import qualified Data.ByteString.Char8         as C
import qualified Data.ByteString.Internal      as B
import qualified Data.ByteString.Unsafe        as B
import           Data.Char
import qualified Data.Foldable                 as F
import           Data.Function
import           Data.Functor.Identity
import qualified Data.IntMap.Strict            as IM
import qualified Data.IntSet                   as IS
import qualified Data.List                     as L
import qualified Data.Map.Strict               as M
import           Data.Monoid
import           Data.Ord
import           Data.Primitive
import           Data.Proxy
import           Data.Ratio
import           Data.Semigroup
import qualified Data.Set                      as S
import           Data.Tuple
import qualified Data.Vector                   as V
import qualified Data.Vector.Generic           as G
import qualified Data.Vector.Generic.Mutable   as GM
import qualified Data.Vector.Mutable           as VM
import qualified Data.Vector.Primitive         as P
import qualified Data.Vector.Primitive.Mutable as PM
import qualified Data.Vector.Unboxed           as U
import qualified Data.Vector.Unboxed.Mutable   as UM
import           Debug.Trace
import           Foreign                       hiding (void)
import           GHC.Exts
import           GHC.TypeLits
import qualified System.IO                     as IO
import           Unsafe.Coerce

#define MOD 1000000007

main :: IO ()
main = do
    [n, m] <- map read.words <$> getLine
    xs <- U.unfoldrN n (runParser int) <$> C.getLine
    print $ solve n m xs

lim :: Int
lim = shiftL 1 17

solve :: Int -> Int -> U.Vector Int -> Int
solve n m xs = snd . U.ifoldr' step (0, 0)
    $ U.zipWith crt
        (convolute p1 3 freq freq)
        (convolute p2 3 freq freq)
  where
    p1 = 998244353
    p2 = 469762049
    !freq = U.accumulate (+) (U.replicate lim 0)
        $ U.map (flip (,) 1) xs
    crt r1 r2 = mod (r1 + p' * (r2 - r1)) (p1 * p2)
    !p' = p1 * recipMod p1 p2 `rem` (p1 * p2)

    step i x (!cnt, !acc)
        | x == 0 || cnt == m = (cnt, acc)
        | cnt + x <= m = (cnt + x, acc + i * x)
        | otherwise = (m, acc + i * (m - cnt))


ntt :: Int -> Int -> U.Vector Int -> U.Vector Int
ntt p g f = runST $ do
    ff <- U.unsafeThaw $ U.unsafeBackpermute f
        $ U.generate n ((`unsafeShiftRL` (64 - logN)) . bitReverse)
    U.forM_ (U.iterateN logN (*2) 2) $ \m -> do
        let !unity = powMod g (quot (p - 1) m) p -- rem (p - 1) m == 0
        let !unities = U.iterateN (unsafeShiftRL m 1) ((`rem` p) . (* unity)) 1
        fix (\loop !k -> when (k < n) $ do
            flip U.imapM_ unities $ \j w -> do
                u <- UM.unsafeRead ff (k + j)
                t <- (* w) <$!> UM.unsafeRead ff (k + j + unsafeShiftRL m 1)
                UM.unsafeWrite ff (k + j) $ rem (u + t) p
                UM.unsafeWrite ff (k + j + unsafeShiftRL m 1) $ mod (u - t) p
            loop (k + m)
            ) 0
    U.unsafeFreeze ff
  where
    !n = U.length f
    !logN = countTrailingZeros n
{-# INLINE ntt #-}

intt :: Int -> Int -> U.Vector Int -> U.Vector Int
intt p g f = U.map ((`rem` p) . (* n')) $ ntt p (recipMod g p) f
  where
    !n' = recipMod (U.length f) p
{-# INLINE intt #-}

convolute :: Int -> Int -> U.Vector Int -> U.Vector Int -> U.Vector Int
convolute p g xs ys
    = intt p g
    $ U.zipWith (\x y -> x * y `rem` p)
        (ntt p g $ xs U.++ U.replicate n 0)
        (ntt p g $ ys U.++ U.replicate n 0)
  where
    !n = U.length xs
{-# INLINE convolute #-}

bitReverse :: Int -> Int
bitReverse
    = unsafeCoerce @Word64 @Int
    . step 32 0xffffffff00000000 0x00000000ffffffff
    . step 16 0xffff0000ffff0000 0x0000ffff0000ffff
    . step 08 0xff00ff00ff00ff00 0x00ff00ff00ff00ff
    . step 04 0xf0f0f0f0f0f0f0f0 0x0f0f0f0f0f0f0f0f
    . step 02 0xcccccccccccccccc 0x3333333333333333
    . step 01 0xaaaaaaaaaaaaaaaa 0x5555555555555555
    . unsafeCoerce @Int @Word64
  where
    step :: Int -> Word64 -> Word64 -> Word64 -> Word64
    step i ml mr = \ !x -> unsafeShiftR (x .&. ml) i .|. unsafeShiftL (x .&. mr) i
    {-# INLINE step #-}

powMod :: (Integral a) => a -> Int -> a -> a
powMod x n m
    | n > 0 = go 1 x n
    | n == 0 = 1
    | otherwise = go 1 (recipMod x m) (-n)
  where
    go !acc !y !i
        | i .&. 1 == 0 = go acc (y * y `rem` m) (unsafeShiftR i 1)
        | i == 1 = acc * y `mod` m
        | otherwise = go (acc * y `rem` m) (y * y `rem` m) (unsafeShiftR (i - 1) 1)
{-# INLINE powMod #-}

recipMod :: (Integral a) => a -> a -> a
recipMod x m = go x m 1 0
  where
    go !a !b !u !v
        | b > 0 = case a `quot` b of
            q -> go b (a - (q * b)) v (u - (q * v))
        | otherwise = u `mod` m
{-# INLINE recipMod #-}

-------------------------------------------------------------------------------
-- Utils
-------------------------------------------------------------------------------
rep :: Monad m => Int -> (Int -> m ()) -> m ()
rep !n = U.forM_ $ U.generate n id
{-# INLINE rep #-}
rev :: Monad m => Int -> (Int -> m ()) -> m ()
rev !n = U.forM_ $ U.iterateN n (subtract 1) (n - 1)
{-# INLINE rev #-}
infixl 8 `shiftRL`, `unsafeShiftRL`
shiftRL = unsafeShiftRL
{-# INLINE shiftRL #-}
unsafeShiftRL (I# x#) (I# i#) = I# (uncheckedIShiftRL# x# i#)
{-# INLINE unsafeShiftRL #-}
type Parser a = StateT C.ByteString Maybe a
runParser :: Parser a -> C.ByteString -> Maybe (a, C.ByteString)
runParser = runStateT
{-# INLINE runParser #-}
int :: Parser Int
int = coerce $ C.readInt . C.dropWhile isSpace
{-# INLINE int #-}
int1 :: Parser Int
int1 = fmap (subtract 1) int
{-# INLINE int1 #-}
char :: Parser Char
char = coerce C.uncons
{-# INLINE char #-}
byte :: Parser Word8
byte = coerce B.uncons
{-# INLINE byte #-}
