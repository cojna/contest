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
import           Data.Monoid                       hiding (First (..),
                                                    Last (..))
import           Data.Ord
import           Data.Primitive
import           Data.Proxy
import           Data.Ratio
import           Data.Semigroup
import qualified Data.Set                          as S
import           Data.Tuple
import qualified Data.Vector                       as V
import qualified Data.Vector.Algorithms.Intro      as Intro
import qualified Data.Vector.Fusion.Bundle.Monadic as MB
import qualified Data.Vector.Fusion.Stream.Monadic as MS
import           Data.Vector.Fusion.Util
import qualified Data.Vector.Generic               as G
import qualified Data.Vector.Generic.Mutable       as GM
import qualified Data.Vector.Mutable               as VM
import qualified Data.Vector.Primitive             as P
import qualified Data.Vector.Primitive.Mutable     as PM
import qualified Data.Vector.Unboxed               as U
import qualified Data.Vector.Unboxed.Mutable       as UM
import           Debug.Trace
import           Foreign                           hiding (void)
import           GHC.Exts
import           GHC.TypeLits
import           System.IO
import           Unsafe.Coerce

#define MOD 1000000007 /* 998244353 */

main :: IO ()
main = do
    [n, t] <- map read.words <$> getLine
    xs <- U.unfoldrN n (runParser int) <$> C.getLine
    print $ solve n t xs

solve :: Int -> Int -> U.Vector Int -> Int
solve n t xs = U.maximum $ U.map calc rsums
  where
    (!ls, !rs) = U.splitAt 20 xs
    calc r = (r+)
        . U.unsafeIndex lsums
        . subtract 1
        $ binarySearch 0 (U.length lsums) $ ((t-r)<) . U.unsafeIndex lsums
    !lsums = radixSort
        . U.filter (<= t)
        . U.map (\bs -> U.sum $ U.ifilter (const . flip memberBS bs) ls)
        $ U.generate (shiftL 1 $ U.length ls) BitSet
    rsums = radixSort
        . U.filter (<= t)
        . U.map (\bs -> U.sum $ U.ifilter (const . flip memberBS bs) rs)
        $ U.generate (shiftL 1 $ U.length rs) BitSet

-------------------------------------------------------------------------------
-- Utils
-------------------------------------------------------------------------------
rep :: (Monad m) => Int -> (Int -> m ()) -> m ()
rep n = flip MS.mapM_ (stream 0 n)
{-# INLINE rep #-}
rep1 :: (Monad m) => Int -> (Int -> m ()) -> m ()
rep1 n = flip MS.mapM_ (stream 1 (n + 1))
{-# INLINE rep1 #-}
rev :: (Monad m) => Int -> (Int -> m ()) -> m ()
rev n = flip MS.mapM_ (streamR 0 n)
{-# INLINE rev #-}
rev1 :: (Monad m) => Int -> (Int -> m ()) -> m ()
rev1 n = flip MS.mapM_ (streamR 1 (n + 1))
{-# INLINE rev1 #-}
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
binarySearchM :: (Monad m) => Int -> Int -> (Int -> m Bool) -> m Int
binarySearchM low high p = go low high where { go !low !high | high <= low = return high | otherwise = p mid >>= bool (go (mid + 1) high) (go low mid) where { mid = low + unsafeShiftRL (high - low) 1}}
{-# INLINE binarySearchM #-}
binarySearch :: Int -> Int -> (Int -> Bool) -> Int
binarySearch low high p = runIdentity (binarySearchM low high (return . p))
{-# INLINE binarySearch #-}
radixSort :: U.Vector Int -> U.Vector Int
radixSort v = F.foldl' step v [0, 16, 32, 48] where { mask k x = unsafeShiftRL x k .&. 65535; step v k = U.create $ do { pos <- UM.unsafeNew 65537; UM.set pos 0; U.forM_ v $ \ x -> do { UM.unsafeModify pos (+ 1) (mask k x + 1)}; rep 65535 $ \ i -> do { fi <- UM.unsafeRead pos i; UM.unsafeModify pos (+ fi) (i + 1)}; res <- UM.unsafeNew $ U.length v; U.forM_ v $ \ x -> do { let { !masked = mask k x}; i <- UM.unsafeRead pos masked; UM.unsafeWrite pos masked $ i + 1; UM.unsafeWrite res i x}; return res}}
{-# INLINE radixSort #-}
encode32x2 :: Int -> Int -> Int
encode32x2 x y = unsafeShiftL x 32 .|. y
{-# INLINE encode32x2 #-}
decode32x2 :: Int -> (Int, Int)
decode32x2 xy = let { !x = unsafeShiftRL xy 32; !y = xy .&. 4294967295} in (x, y)
{-# INLINE decode32x2 #-}
-------------------------------------------------------------------------------
-- Data.BitSet
-------------------------------------------------------------------------------
newtype BitSet = BitSet{getBitSet :: Int} deriving (Eq, Ord)
instance Show BitSet where { showsPrec p xs = showParen (p > 10) $ showString "fromList " . shows (toList xs)}
instance IsList BitSet where { type Item BitSet = Int; fromList = BitSet . F.foldl' (\ acc x -> acc .|. unsafeShiftL 1 x) 0; toList bs = filter (`memberBS` bs) [0 .. 63]}
emptyBS :: BitSet
emptyBS = BitSet 0
singletonBS :: Int -> BitSet
singletonBS (I# i#) = BitSet (I# (uncheckedIShiftL# 1# i#))
insertBS :: Int -> BitSet -> BitSet
insertBS (I# i#) (BitSet (I# bs#)) = BitSet (I# ((uncheckedIShiftL# 1# i#) `orI#` bs#))
deleteBS :: Int -> BitSet -> BitSet
deleteBS (I# i#) (BitSet (I# bs#)) = BitSet (I# (notI# (uncheckedIShiftL# 1# i#) `andI#` bs#))
memberBS :: Int -> BitSet -> Bool
memberBS (I# i#) (BitSet (I# bs#)) = isTrue# (uncheckedIShiftRL# bs# i# `andI#` 1#)
notMemberBS :: Int -> BitSet -> Bool
notMemberBS i = not . memberBS i
nullBS :: BitSet -> Bool
nullBS = (== 0) . coerce @BitSet @Int
sizeBS :: BitSet -> Int
sizeBS = coerce (popCount @Int)
isSubsetOf :: BitSet -> BitSet -> Bool
isSubsetOf x y = intersectionBS x y == x
unionBS :: BitSet -> BitSet -> BitSet
unionBS = coerce ((.|.) @Int)
complementBS :: BitSet -> BitSet
complementBS = coerce (complement @Int)
differenceBS :: BitSet -> BitSet -> BitSet
differenceBS x y = intersectionBS x (complementBS y)
intersectionBS :: BitSet -> BitSet -> BitSet
intersectionBS = coerce ((.&.) @Int)
findMinBS :: BitSet -> Int
findMinBS = coerce (countTrailingZeros @Int)
findMaxBS :: BitSet -> Int
findMaxBS = (63 -) . coerce (countLeadingZeros @Int)
deleteMinBS :: BitSet -> BitSet
deleteMinBS (BitSet x) = BitSet (x .&. (x - 1))
deleteMaxBS :: BitSet -> BitSet
deleteMaxBS x = deleteBS (findMaxBS x) x
deleteFindMin :: BitSet -> (Int, BitSet)
deleteFindMin x = (findMinBS x, deleteMinBS x)
deleteFindMax :: BitSet -> (Int, BitSet)
deleteFindMax x = let { i = findMaxBS x} in (i, deleteBS i x)
minView :: BitSet -> Maybe (Int, BitSet)
minView x | x /= BitSet 0 = Just $ deleteFindMin x | otherwise = Nothing
maxView :: BitSet -> Maybe (Int, BitSet)
maxView x | x /= BitSet 0 = Just $ deleteFindMax x | otherwise = Nothing
newtype instance  UM.MVector s BitSet = MV_BitSet (UM.MVector s Int)
newtype instance  U.Vector BitSet = V_BitSet (U.Vector Int)
instance U.Unbox BitSet
instance GM.MVector UM.MVector BitSet where { basicLength (MV_BitSet v) = GM.basicLength v; {-# INLINE basicLength #-}; basicUnsafeSlice i n (MV_BitSet v) = MV_BitSet $ GM.basicUnsafeSlice i n v; {-# INLINE basicUnsafeSlice #-}; basicOverlaps (MV_BitSet v1) (MV_BitSet v2) = GM.basicOverlaps v1 v2; {-# INLINE basicOverlaps #-}; basicUnsafeNew n = MV_BitSet `liftM` GM.basicUnsafeNew n; {-# INLINE basicUnsafeNew #-}; basicInitialize (MV_BitSet v) = GM.basicInitialize v; {-# INLINE basicInitialize #-}; basicUnsafeReplicate n x = MV_BitSet `liftM` GM.basicUnsafeReplicate n (coerce x); {-# INLINE basicUnsafeReplicate #-}; basicUnsafeRead (MV_BitSet v) i = coerce `liftM` GM.basicUnsafeRead v i; {-# INLINE basicUnsafeRead #-}; basicUnsafeWrite (MV_BitSet v) i x = GM.basicUnsafeWrite v i (coerce x); {-# INLINE basicUnsafeWrite #-}; basicClear (MV_BitSet v) = GM.basicClear v; {-# INLINE basicClear #-}; basicSet (MV_BitSet v) x = GM.basicSet v (coerce x); {-# INLINE basicSet #-}; basicUnsafeCopy (MV_BitSet v1) (MV_BitSet v2) = GM.basicUnsafeCopy v1 v2; {-# INLINE basicUnsafeCopy #-}; basicUnsafeMove (MV_BitSet v1) (MV_BitSet v2) = GM.basicUnsafeMove v1 v2; {-# INLINE basicUnsafeMove #-}; basicUnsafeGrow (MV_BitSet v) n = MV_BitSet `liftM` GM.basicUnsafeGrow v n; {-# INLINE basicUnsafeGrow #-}}
instance G.Vector U.Vector BitSet where { basicUnsafeFreeze (MV_BitSet v) = V_BitSet `liftM` G.basicUnsafeFreeze v; {-# INLINE basicUnsafeFreeze #-}; basicUnsafeThaw (V_BitSet v) = MV_BitSet `liftM` G.basicUnsafeThaw v; {-# INLINE basicUnsafeThaw #-}; basicLength (V_BitSet v) = G.basicLength v; {-# INLINE basicLength #-}; basicUnsafeSlice i n (V_BitSet v) = V_BitSet $ G.basicUnsafeSlice i n v; {-# INLINE basicUnsafeSlice #-}; basicUnsafeIndexM (V_BitSet v) i = coerce `liftM` G.basicUnsafeIndexM v i; {-# INLINE basicUnsafeIndexM #-}; basicUnsafeCopy (MV_BitSet mv) (V_BitSet v) = G.basicUnsafeCopy mv v; elemseq _ = seq; {-# INLINE elemseq #-}}
