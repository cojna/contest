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
import           Foreign                           hiding (void)
import           GHC.Exts
import           GHC.TypeLits
import           System.IO
import           Unsafe.Coerce

#define MOD 1000000007

main :: IO ()
main = do
    n <- readLn @Int
    ps <- U.unfoldrN n (runParser $ P <$> int <*> int) <$> C.getContents
    case solve n ps of
        (x, y, z) -> putStrLn.unwords$map show[x,y,z]

double :: Parser Double
double = fmap fromIntegral int

pointDouble :: Parser (Point Double)
pointDouble = P <$> double <*> double

arg :: (Integral a) => Point a -> Double
arg (P x y) = atan2 (fromIntegral y) (fromIntegral x)
{-# SPECIALISE arg :: Point Int -> Double #-}

solve :: Int -> U.Vector (Point Int) -> (Int, Int, Int)
solve n ps0 = answer . U.foldl' merge (0, 0) $ U.imap ocalc ps0
  where
    answer (a, b) = (n*(n-1)*(n-2)`div`6-a-b,a,b)
    merge (!x0,!y0) (!x1,!y1) = (x0+x1,y0+y1)
    ocalc i o = U.foldl' merge (0, 0) . U.imap icalc $ U.take (n - 1) sorted
      where
        ps = U.map snd
            . U.modify (Intro.sortBy (comparing fst))
            . U.map (\p -> let p' = p - o in (arg p', p'))
            $ U.ifilter (\j _ -> j /= i) ps0
        !sorted = ps U.++ ps

        icalc i p
            | qi == h = (0, 0)
            | dot p (sorted U.! qi) == 0 = (1, h - qi - 1)
            | otherwise = (0, h - qi)
          where
            offset = i + 1
            !h = lowerBound offset (offset + n - 2) $ \j -> cross p (sorted `U.unsafeIndex` j) <= 0
            qi = lowerBound offset h $ \j -> dot p (sorted `U.unsafeIndex` j) <= 0

newtype EPS a = EPS {getEPS :: a}
    deriving newtype (Num, Fractional)

eps :: (Fractional a) => a
eps = 1e-8

instance (Num a, Ord a, Fractional a) => Eq (EPS a) where
    (EPS x) == (EPS y) = abs (y - x) < eps

instance (Num a, Ord a, Fractional a) => Ord (EPS a) where
    (EPS x) < (EPS y) = x < y - eps
    (EPS x) <= (EPS y) = x < y + eps
    (EPS x) > (EPS y) = x > y + eps
    (EPS x) >= (EPS y) = x > y - eps

data Point a = P !a !a

instance (Show a) => Show (Point a) where
    show (P x y) = shows x $ ' ':show y

instance Functor Point where
    fmap f (P x y) = P (f x) (f y)

instance (Eq a) => Eq (Point a) where
    (P x0 y0) == (P x1 y1) = x0 == x1 && y0 == y1

instance (Ord a) => Ord (Point a) where
    compare (P x0 y0) (P x1 y1) = compare (x0, y0) (x1, y1)

instance (Num a) => Num (Point a) where
    {-# SPECIALISE instance Num (Point Int) #-}
    {-# SPECIALISE instance Num (Point Integer) #-}
    {-# SPECIALISE instance Num (Point Double) #-}
    {-# SPECIALISE instance Num (Point (EPS Double)) #-}
    (P x0 y0) + (P x1 y1) = P (x0 + x1) (y0 + y1)
    (P x0 y0) - (P x1 y1) = P (x0 - x1) (y0 - y1)
    (P x0 y0) * (P x1 y1) = P (x0 * x1 - y0 * y1) (x0 * y1 + x1 * y0)
    negate = fmap negate
    abs = id
    signum _ = P 1 0
    fromInteger n = P (fromInteger n) 0

dot :: (Num a) => Point a -> Point a -> a
dot (P x0 y0) (P x1 y1) = x0 * x1 + y0 * y1
{-# INLINE dot #-}

cross :: (Num a) => Point a -> Point a -> a
cross (P x0 y0) (P x1 y1) = x0 * y1 - y0 * x1
{-# INLINE cross #-}

conjugate :: (Num a) => Point a -> Point a
conjugate (P x y) = P x (-y)
{-# INLINE conjugate #-}

area :: (Num a) => Point a -> Point a -> Point a -> a
area o u v = cross (u - o) (v - o)
{-# INLINE area #-}

compareCCW :: (Num a, Ord a) => Point a -> Point a -> Point a -> Ordering
compareCCW o = \u v -> compare 0 (area o u v)
{-# INLINE compareCCW #-}

compareCW :: (Num a, Ord a) => Point a -> Point a -> Point a-> Ordering
compareCW o = flip (compareCCW o)
{-# INLINE compareCW #-}

newtype instance UM.MVector s (Point a) = MV_Point (UM.MVector s a)
newtype instance U.Vector (Point a) = V_Point (U.Vector a)

instance (U.Unbox a) => U.Unbox (Point a)

instance (U.Unbox a) => GM.MVector UM.MVector (Point a) where
    basicLength (MV_Point v) = unsafeShiftR (GM.basicLength v) 1
    {-# INLINE basicLength #-}
    basicUnsafeSlice i n (MV_Point v) = MV_Point $ GM.basicUnsafeSlice (2 * i) (2 * n) v
    {-# INLINE basicUnsafeSlice #-}
    basicOverlaps (MV_Point v1) (MV_Point v2) = GM.basicOverlaps v1 v2
    {-# INLINE basicOverlaps #-}
    basicUnsafeNew n = MV_Point `liftM` GM.basicUnsafeNew (2 * n)
    {-# INLINE basicUnsafeNew #-}
    basicInitialize (MV_Point v) = GM.basicInitialize v
    {-# INLINE basicInitialize #-}
    basicUnsafeRead (MV_Point v) i = P `liftM` GM.basicUnsafeRead v (2 * i) `ap` GM.basicUnsafeRead v (2 * i + 1)
    {-# INLINE basicUnsafeRead #-}
    basicUnsafeWrite (MV_Point v) i (P x y) = GM.basicUnsafeWrite v (2 * i) x >> GM.basicUnsafeWrite v (2 * i + 1) y
    {-# INLINE basicUnsafeWrite #-}
    basicClear (MV_Point v) = GM.basicClear v
    {-# INLINE basicClear #-}
    basicUnsafeCopy (MV_Point v1) (MV_Point v2) = GM.basicUnsafeCopy v1 v2
    {-# INLINE basicUnsafeCopy #-}
    basicUnsafeMove (MV_Point v1) (MV_Point v2) = GM.basicUnsafeMove v1 v2
    {-# INLINE basicUnsafeMove #-}
    basicUnsafeGrow (MV_Point v) n = MV_Point `liftM` GM.basicUnsafeGrow v (2 * n)
    {-# INLINE basicUnsafeGrow #-}

instance (U.Unbox a) => G.Vector U.Vector (Point a) where
    basicUnsafeFreeze (MV_Point v) = V_Point `liftM` G.basicUnsafeFreeze v
    {-# INLINE basicUnsafeFreeze #-}
    basicUnsafeThaw (V_Point v) = MV_Point `liftM` G.basicUnsafeThaw v
    {-# INLINE basicUnsafeThaw #-}
    basicLength (V_Point v) = unsafeShiftR (G.basicLength v) 1
    {-# INLINE basicLength #-}
    basicUnsafeSlice i n (V_Point v) = V_Point $ G.basicUnsafeSlice (2 * i) (2 * n) v
    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeIndexM (V_Point v) i = P `liftM` G.basicUnsafeIndexM v (2 * i) `ap` G.basicUnsafeIndexM v (2 * i + 1)
    {-# INLINE basicUnsafeIndexM #-}
    basicUnsafeCopy (MV_Point mv) (V_Point v) = G.basicUnsafeCopy mv v
    elemseq _ = seq
    {-# INLINE elemseq #-}


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
