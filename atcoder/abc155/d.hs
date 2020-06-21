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
    [n, k] <- map read.words <$> getLine :: IO [Int]
    xs <- U.unfoldrN n (runParser int) <$> C.getLine
    print $ solve n k xs

offset :: Int
offset = 1000000000

solve :: Int -> Int -> U.Vector Int -> Int
solve n k xs = lowerBound minBound maxBound ((k<=) . count)
  where
    !poss = radixSortInt $ U.filter (>0) xs
    !zeros = U.filter (==0) xs
    !negs = U.map (subtract offset) . radixSortInt . U.map (+offset) $ U.filter (<0) xs

    !zero = U.length zeros * (n - U.length zeros) + U.length zeros * (U.length zeros - 1) `div` 2
    !neg = U.length poss * U.length negs

    countPP :: Int -> Int -> Int -> Int
    countPP lim i x
        | U.null v || x * U.head v > lim = 0
        | otherwise = succ.upperBound 0 (U.length v - 1) $ \j ->
           x * U.unsafeIndex v j <= lim
      where
        !v = U.drop (i + 1) poss

    countNN :: Int -> Int -> Int -> Int
    countNN lim i x
        | U.null v || x * U.last v > lim = 0
        | otherwise = (U.length v-).lowerBound 0 (U.length v - 1) $ \i ->
            x * U.unsafeIndex v i <= lim
      where
        !v = U.drop (i + 1) negs

    countNP :: Int -> Int -> Int -> Int
    countNP lim i x
        | U.null v || x * U.last v > lim = 0
        | otherwise = (U.length v-).lowerBound 0 (U.length v - 1) $ \i ->
            x * U.unsafeIndex v i <= lim
      where
        !v = poss

    -- less than or equal equal to lim
    count :: Int -> Int
    count lim = case compare lim 0 of
        GT -> neg + zero
            + (U.sum $ U.imap (countPP lim) poss)
            + (U.sum $ U.imap (countNN lim) negs)
        EQ -> neg + zero
        LT -> U.sum $ U.imap (countNP lim) negs

-- | assert (p high)
lowerBound :: Int -> Int -> (Int -> Bool) -> Int
lowerBound low high p = go low high
  where
    go !low !high
        | high <= low = high
        | p mid       = go low       mid
        | otherwise   = go (mid + 1) high
      where
        mid = low + unsafeShiftRL (high - low) 1
{-# INLINE lowerBound #-}

-- | assert (p low)
upperBound :: Int -> Int -> (Int -> Bool) -> Int
upperBound low high p
    | p high = high
    | otherwise = lowerBound low high (not.p) - 1
{-# INLINE upperBound #-}

radixSortInt :: U.Vector Int -> U.Vector Int
radixSortInt = unsafeCoerce.radixSort64.unsafeCoerce

radixSort64 :: U.Vector Word64 -> U.Vector Word64
radixSort64 v = F.foldl' step v [0, 16, 32, 48]
  where
    mask k x = fromIntegral $ unsafeShiftR x k .&. 0xffff
    step v k = U.create $ do
        pref <- U.unsafeThaw
            . U.prescanl' (+) 0
            . U.unsafeAccumulate (+) (U.replicate 0x10000 0)
            $ U.map (flip (,) 1 . mask k) v
        res <- UM.unsafeNew $ U.length v
        U.forM_ v $ \x -> do
            let !masked = mask k x
            i <- UM.unsafeRead pref masked
            UM.unsafeWrite pref masked $ i + 1
            UM.unsafeWrite res i x
        return res
{-# INLINE radixSort64 #-}

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
