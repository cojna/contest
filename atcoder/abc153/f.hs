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
    [n, d, a] <- map read.words <$> getLine :: IO [Int]
    xhs <- U.unfoldrN n (runParser $ (,) <$> int <*> int) <$> C.getContents
    print $ solve n d a xhs

encode :: (Int, Int) -> Word64
encode (x, y) = unsafeCoerce $ unsafeShiftL x 32 + y

decode :: Word64 -> (Int, Int)
decode xy = unsafeCoerce (x, y)
  where
    !x = unsafeShiftR xy 32 .&. 0xffffffff
    !y = xy .&. 0xffffffff

solve :: Int -> Int -> Int -> U.Vector (Int, Int) -> Int
solve n d a (U.map decode.radixSort64.U.map encode -> xhs) = runST $ do
    damages <- newFenwickTree (n+1)
    res <- UM.replicate 1 0
    flip U.imapM xhs $ \i (x, h) -> do
        damage <- sumTo (i + 1) damages
        when (damage < h) $ do
            let q = quot (h - damage + a - 1) a
            UM.unsafeModify res (+q) 0
            let end = lowerBound (i + 1) n (\j ->
                    fst (U.unsafeIndex xhs j) > x + 2 * d
                    )
            addAt i (q * a) damages
            addAt end (- q * a) damages
    UM.read res 0

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
-------------------------------------------------------------------------------
-- Data.FenwickTree
-------------------------------------------------------------------------------
newtype FenwickTree s a = FenwickTree{getFenwickTree :: UM.MVector s a}
newFenwickTree :: (PrimMonad m, U.Unbox a, Num a) => Int -> m (FenwickTree (PrimState m) a)
newFenwickTree n = FenwickTree <$> UM.replicate (n + 1) 0
buildFenwickTree :: (PrimMonad m, U.Unbox a, Num a) => U.Vector a -> m (FenwickTree (PrimState m) a)
buildFenwickTree vec = do { let { n = U.length vec}; ft <- UM.replicate (n + 1) 0; U.unsafeCopy (UM.tail ft) vec; U.forM_ (U.generate n (+ 1)) $ \ i -> do { let { j = i + (i .&. (-i))}; when (j <= n) $ do { fti <- UM.unsafeRead ft i; UM.unsafeModify ft (+ fti) j}}; return $ FenwickTree ft}
getFreezeFenwickTree :: (PrimMonad m, U.Unbox a) => FenwickTree (PrimState m) a -> m (U.Vector a)
getFreezeFenwickTree (FenwickTree ft) = do { U.freeze ft}
sumTo :: (PrimMonad m, U.Unbox a, Num a) => Int -> FenwickTree (PrimState m) a -> m a
sumTo k (FenwickTree ft) = go 0 k where { go !acc !i | i > 0 = do { xi <- UM.unsafeRead ft i; go (acc + xi) (i - (i .&. (-i)))} | otherwise = return acc}
{-# INLINE sumTo #-}
sumFromTo :: (PrimMonad m, U.Unbox a, Num a) => Int -> Int -> FenwickTree (PrimState m) a -> m a
sumFromTo l r ft = (-) <$> sumTo r ft <*> sumTo l ft
{-# INLINE sumFromTo #-}
addAt :: (PrimMonad m, U.Unbox a, Num a) => Int -> a -> FenwickTree (PrimState m) a -> m ()
addAt k v (FenwickTree ft) = flip fix (k + 1) $ \ loop !i -> do { when (i < n) $ do { UM.unsafeModify ft (+ v) i; loop $ i + (i .&. (-i))}} where { n = UM.length ft}
{-# INLINE addAt #-}
findMaxIndexLT :: (PrimMonad m, U.Unbox a, Num a, Ord a) => a -> FenwickTree (PrimState m) a -> m Int
findMaxIndexLT w0 (FenwickTree ft) | w0 <= 0 = return 0 | otherwise = go w0 highestOneBit 0 where { n = UM.length ft; highestOneBit = until (> n) (* 2) 1 `quot` 2; go !w !step !i | step == 0 = return i | otherwise = do { if i + step < n then do { u <- UM.unsafeRead ft (i + step); if u < w then go (w - u) (step `unsafeShiftR` 1) (i + step) else go w (step `unsafeShiftR` 1) i} else go w (step `unsafeShiftR` 1) i}}
{-# INLINE findMaxIndexLT #-}
