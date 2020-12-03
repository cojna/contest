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
    n <- readLn @Int
    qs <- U.unfoldrN n (runParser $ (,) <$> int1 <*> int) <$> C.getContents
    putStr.unlines.map show $ solve n qs

solve :: Int -> U.Vector (Int, Int) -> [Int]
solve n (U.modify Intro.sort -> qs0) = go 0 0 emptyIMS $ U.toList qs0
    where
        go !res !t !h ((a,b):qs)
            | a <= t = go res t (insertIMS b h) qs
        go !res !t !h qs
            | Just (x, h') <- maxViewIMS h =
                let !res' = res + x
                in res' : go res' (t + 1) h' qs
            | otherwise = []


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
-- Data.IntMultiSet
-------------------------------------------------------------------------------
newtype IntMultiSet = IntMultiSet{getIntMultiSet :: IM.IntMap Int} deriving (Eq)
instance Show IntMultiSet where { show = show . toList}
instance IsList IntMultiSet where { type Item IntMultiSet = Int; fromList = L.foldl' (flip insertIMS) emptyIMS; toList = concatMap (\ (k, x) -> replicate x k) . IM.toList . (coerce :: IntMultiSet -> IM.IntMap Int)}
emptyIMS :: IntMultiSet
emptyIMS = coerce (IM.empty :: IM.IntMap Int)
singletonIMS :: Int -> IntMultiSet
singletonIMS x = coerce $ IM.singleton x (1 :: Int)
insertIMS :: Int -> IntMultiSet -> IntMultiSet
insertIMS x = coerce . IM.insertWith (+) x (1 :: Int) . coerce
deleteIMS :: Int -> IntMultiSet -> IntMultiSet
deleteIMS x = (coerce :: IM.IntMap Int -> IntMultiSet) . IM.update (\ y -> if y > 1 then Just $! y - 1 else Nothing) x . coerce
deleteAllIMS :: Int -> IntMultiSet -> IntMultiSet
deleteAllIMS x = (coerce :: IM.IntMap Int -> IntMultiSet) . IM.delete x . coerce
memberIMS :: Int -> IntMultiSet -> Bool
memberIMS x = IM.member x . (coerce :: IntMultiSet -> IM.IntMap Int)
notMemberIMS :: Int -> IntMultiSet -> Bool
notMemberIMS x = not . memberIMS x
countIMS :: Int -> IntMultiSet -> Int
countIMS key = IM.findWithDefault 0 key . coerce
lookupLTIMS :: Int -> IntMultiSet -> Maybe Int
lookupLTIMS x = fmap fst . IM.lookupLT x . (coerce :: IntMultiSet -> IM.IntMap Int)
lookupGTIMS :: Int -> IntMultiSet -> Maybe Int
lookupGTIMS x = fmap fst . IM.lookupGT x . (coerce :: IntMultiSet -> IM.IntMap Int)
lookupLEIMS :: Int -> IntMultiSet -> Maybe Int
lookupLEIMS x = fmap fst . IM.lookupLE x . (coerce :: IntMultiSet -> IM.IntMap Int)
lookupGEIMS :: Int -> IntMultiSet -> Maybe Int
lookupGEIMS x = fmap fst . IM.lookupGE x . (coerce :: IntMultiSet -> IM.IntMap Int)
nullIMS :: IntMultiSet -> Bool
nullIMS = IM.null . (coerce :: IntMultiSet -> IM.IntMap Int)
sizeIMS :: IntMultiSet -> Int
sizeIMS = IM.foldl' (+) 0 . (coerce :: IntMultiSet -> IM.IntMap Int)
findMinIMS :: IntMultiSet -> Int
findMinIMS = fst . IM.findMin . (coerce :: IntMultiSet -> IM.IntMap Int)
findMaxIMS :: IntMultiSet -> Int
findMaxIMS = fst . IM.findMax . (coerce :: IntMultiSet -> IM.IntMap Int)
deleteMinIMS :: IntMultiSet -> IntMultiSet
deleteMinIMS = coerce . IM.updateMin (\ x -> if x > 1 then Just $! x - 1 else Nothing) . (coerce :: IntMultiSet -> IM.IntMap Int)
deleteMaxIMS :: IntMultiSet -> IntMultiSet
deleteMaxIMS = coerce . IM.updateMax (\ x -> if x > 1 then Just $! x - 1 else Nothing) . (coerce :: IntMultiSet -> IM.IntMap Int)
maxViewIMS :: IntMultiSet -> Maybe (Int, IntMultiSet)
maxViewIMS = maybe Nothing just . IM.maxViewWithKey . (coerce :: IntMultiSet -> IM.IntMap Int) where { just :: ((Int, Int), IM.IntMap Int) -> Maybe (Int, IntMultiSet); just ((k, x), m) | x > 1 = case IM.insert k (x - 1) m of { m' -> Just (k, coerce m')} | otherwise = Just (k, coerce m)}
minViewIMS :: IntMultiSet -> Maybe (Int, IntMultiSet)
minViewIMS = maybe Nothing just . IM.minViewWithKey . (coerce :: IntMultiSet -> IM.IntMap Int) where { just :: ((Int, Int), IM.IntMap Int) -> Maybe (Int, IntMultiSet); just ((k, x), m) | x > 1 = case IM.insert k (x - 1) m of { m' -> Just (k, coerce m')} | otherwise = Just (k, coerce m)}
