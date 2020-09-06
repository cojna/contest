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
import qualified Data.ByteString                   as B
import qualified Data.ByteString.Builder           as B
import qualified Data.ByteString.Char8             as C
import qualified Data.ByteString.Internal          as B
import qualified Data.ByteString.Unsafe            as B
import           Data.Char
import qualified Data.Foldable                     as F
import           Data.Function
import           Data.Functor.Identity
import qualified Data.IntMap.Strict                as IM
import qualified Data.IntSet                       as IS
import qualified Data.List                         as L
import qualified Data.Map.Strict                   as M
import           Data.Monoid
import           Data.Ord
import           Data.Primitive
import           Data.Proxy
import           Data.Ratio
import           Data.Semigroup
import qualified Data.Set                          as S
import           Data.Tuple
import qualified Data.Vector                       as V
import qualified Data.Vector.Algorithms.Intro      as Intro
import qualified Data.Vector.Fusion.Stream.Monadic as MS
import qualified Data.Vector.Fusion.Bundle.Monadic as MB
import           Data.Vector.Fusion.Util
import qualified Data.Vector.Generic               as G
import qualified Data.Vector.Generic.Mutable       as GM
import qualified Data.Vector.Mutable               as VM
import qualified Data.Vector.Primitive             as P
import qualified Data.Vector.Primitive.Mutable     as PM
import qualified Data.Vector.Unboxed               as U
import qualified Data.Vector.Unboxed.Mutable       as UM
import           Debug.Trace
import           Foreign                           hiding (void, testBit)
import           GHC.Exts
import           GHC.TypeLits
import           System.IO
import           Unsafe.Coerce

#define MOD 1000000007

main :: IO ()
main = do
    n <- readLn @Int
    xs <- U.unfoldrN n (runParser int) <$> C.getLine
    ys <- U.unfoldrN n (runParser int) <$> C.getLine
    print $ solve n xs ys

solve :: Int -> U.Vector Int -> U.Vector Int -> Int
solve n xs ys = U.foldl' (.|.) 0
    $ U.generate 29 (\i -> shiftL (calc i .&. 1) i)
  where
    calc 0 =
        let xs1 = U.length $ U.filter (`testBit` 0) xs
            xs0 = n - xs1
            ys1 = U.length $ U.filter (`testBit` 0) ys
            ys0 = n - ys1
        in xs1 * ys0 + xs0 * ys1
    calc k =
        let !mask = bit k - 1
            !ys0 = radixSort64 . U.map (.&. mask) $ U.filter (not.(`testBit` k)) ys
            !ys1 = radixSort64 . U.map (.&. mask) $ U.filter (`testBit` k) ys
            !num0 = U.length ys0
            !num1 = U.length ys1
            f x | testBit x k = k0 + num1 - k1
                | otherwise = k1 + num0 - k0
              where
                !x' = x .&. mask
                !y' = mask - x'
                !k0 = lowerBound 0 num0 ((y' <).U.unsafeIndex ys0)
                !k1 = lowerBound 0 num1 ((y' <).U.unsafeIndex ys1)
        in U.sum $ U.map f xs


testBit :: Int -> Int -> Bool
testBit x i = unsafeShiftRL x i .&. 1 == 1

radixSort64 :: U.Vector Int -> U.Vector Int
radixSort64 v = F.foldl' step v [0, 16, 32, 48]
  where
    mask k x = fromIntegral $ unsafeShiftRL x k .&. 0xffff
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
rep :: (Monad m) => Int -> (Int -> m ()) -> m ()
rep n = flip MS.mapM_ (stream 0 n)
{-# INLINE rep #-}
rev :: (Monad m) => Int -> (Int -> m ()) -> m ()
rev !n = flip MS.mapM_ (streamR 0 n)
{-# INLINE rev #-}
stream :: (Monad m) => Int -> Int -> MS.Stream m Int
stream !l !r = MS.Stream step l where { step x | x < r = return $ MS.Yield x (x + 1) | otherwise = return MS.Done; {-# INLINE [0] step #-}}
{-# INLINE [1] stream #-}
streamR :: (Monad m) => Int -> Int -> MS.Stream m Int
streamR !l !r = MS.Stream step (r - 1) where { step x | x >= l = return $ MS.Yield x (x - 1) | otherwise = return MS.Done; {-# INLINE [0] step #-}}
{-# INLINE [1] streamR #-}
stream' :: (Monad m) => Int -> Int -> Int -> MS.Stream m Int
stream' !l !r !d = MS.Stream step l where { step x | x < r = return $ MS.Yield x (x + d) | otherwise = return MS.Done; {-# INLINE [0] step #-}}
{-# INLINE [1] stream' #-}
infixl 8 `shiftRL`, `unsafeShiftRL`
shiftRL :: Int -> Int -> Int
shiftRL = unsafeShiftRL
{-# INLINE shiftRL #-}
unsafeShiftRL :: Int -> Int -> Int
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
skipSpaces :: Parser ()
skipSpaces = modify' (C.dropWhile isSpace)
{-# INLINE skipSpaces #-}
lowerBoundM :: (Monad m) => Int -> Int -> (Int -> m Bool) -> m Int
lowerBoundM low high p = go low high where { go !low !high | high <= low = return high | otherwise = p mid >>= bool (go (mid + 1) high) (go low mid) where { mid = low + unsafeShiftRL (high - low) 1}}
{-# INLINE lowerBoundM #-}
upperBoundM :: (Monad m) => Int -> Int -> (Int -> m Bool) -> m Int
upperBoundM low high p = do { flg <- p high; if flg then return high else subtract 1 <$!> lowerBoundM low high (fmap not . p)}
{-# INLINE upperBoundM #-}
lowerBound :: Int -> Int -> (Int -> Bool) -> Int
lowerBound low high p = runIdentity (lowerBoundM low high (return . p))
{-# INLINE lowerBound #-}
upperBound :: Int -> Int -> (Int -> Bool) -> Int
upperBound low high p = runIdentity (upperBoundM low high (return . p))
{-# INLINE upperBound #-}
