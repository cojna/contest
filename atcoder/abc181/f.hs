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
    ps <- U.unfoldrN n (runParser point) <$> C.getContents
    print $ solve n ps

point :: (Num a) => Parser (Point a)
point = P <$> (fromIntegral <$> int) <*> (fromIntegral <$> int)

solve :: Int -> U.Vector (Point Int)-> Double
solve n ps
    = (0.5 *) . sqrt . fromIntegral
    . maximum
    . map (gr U.!)
    $ zipWith ix path (tail path)
  where
    gr :: U.Vector Int
    !gr = U.create $ do
        mat <- UM.new ((n + 2) * (n + 2))
        rep n $ \i -> do
            rep n $ \j -> do
                UM.write mat (ix i j)
                    $ sqrNorm (ps U.! i - ps U.! j)
        rep n $ \i -> do
            let P _ yi = ps U.! i
            let !dsrc = (100 - yi) ^ 2
            let !dsink = (100 + yi) ^ 2
            UM.write mat (ix src i) dsrc
            UM.write mat (ix i src) dsrc
            UM.write mat (ix sink i) dsink
            UM.write mat (ix i sink) dsink
        UM.write mat (ix src sink) $ 200 ^ 2
        UM.write mat (ix sink src) $ 200 ^ 2
        return mat
    !parent = primDense (n + 2) src gr
    path = reverse . takeWhile (>=0) $ iterate (parent U.!) sink
    ix i j = i * (n + 2) + j
    src = n
    sink = n + 1

primDense :: (U.Unbox w, Num w, Ord w)
    => Int -- ^ n
    -> Int -- ^ root
    -> U.Vector w -- ^ adjacent matrix (n x n)
    -> U.Vector Int -- ^ parent (parent[root] = -1)
primDense n root gr
    | root >= n || U.length gr /= n * n
        = error "primDense: Invalid Arguments"
    | otherwise = U.create $ do
        let !inf = 2 * U.maximum gr
        parent <- UM.replicate n (-1)
        dist <- UM.new n
        used <- UM.replicate n False
        UM.write dist root 0
        UM.write used root True
        rep n $ \i -> do
            when (i /= root) $ do
                UM.write parent i root
            UM.write dist i $ U.unsafeIndex gr (root * n + i)
        rep (n - 1) $ \_ -> do
            v <- fmap snd $ MS.foldM'
                (\acc i -> do
                    UM.unsafeRead used i >>= \case
                        False -> do
                            d <- UM.unsafeRead dist i
                            return $! min acc (d, i)
                        True -> return acc
                ) (inf, (-1)) $ stream 0 n
            UM.write used v True
            rep n $ \u -> do
                UM.unsafeRead used u >>= \case
                    False -> do
                        du <- UM.unsafeRead dist u
                        let dvu = U.unsafeIndex gr (v * n + u)
                        when (dvu < du) $ do
                            UM.unsafeWrite dist u dvu
                            UM.unsafeWrite parent u v
                    True -> return ()
        return parent
{-# INLINE primDense #-}



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
-- Data.UnionFind
-------------------------------------------------------------------------------
newtype UnionFind s = UF{getUnionFind :: UM.MVector s Int}
newUnionFind :: PrimMonad m => Int -> m (UnionFind (PrimState m))
newUnionFind n = UF <$> UM.replicate n (-1)
{-# INLINE newUnionFind #-}
freezeUnionFind :: PrimMonad m => UnionFind (PrimState m) -> m (U.Vector Int)
freezeUnionFind = U.freeze . getUnionFind
findUF :: PrimMonad m => UnionFind (PrimState m) -> Int -> m Int
findUF uf x = go x return where { go !x k = do { px <- UM.unsafeRead (getUnionFind uf) x; if px < 0 then k x else go px $ \ ppx -> do { UM.unsafeWrite (getUnionFind uf) x ppx; k ppx}}}
{-# INLINE findUF #-}
sizeUF :: PrimMonad m => UnionFind (PrimState m) -> Int -> m Int
sizeUF uf = fix $ \ loop x -> do { px <- UM.unsafeRead (getUnionFind uf) x; if px < 0 then return $! negate px else loop px}
{-# INLINE sizeUF #-}
uniteUF :: PrimMonad m => UnionFind (PrimState m) -> Int -> Int -> m Bool
uniteUF uf x y = do { px <- findUF uf x; py <- findUF uf y; if px == py then return False else do { rx <- UM.unsafeRead (getUnionFind uf) px; ry <- UM.unsafeRead (getUnionFind uf) py; if rx < ry then do { UM.unsafeModify (getUnionFind uf) (+ ry) px; UM.unsafeWrite (getUnionFind uf) py px} else do { UM.unsafeModify (getUnionFind uf) (+ rx) py; UM.unsafeWrite (getUnionFind uf) px py}; return True}}
{-# INLINE uniteUF #-}
uniteUF_ :: PrimMonad m => UnionFind (PrimState m) -> Int -> Int -> m ()
uniteUF_ uf x y = void $ uniteUF uf x y
{-# INLINE uniteUF_ #-}
equivUF :: PrimMonad m => UnionFind (PrimState m) -> Int -> Int -> m Bool
equivUF uf x y = (==) `liftM` findUF uf x `ap` findUF uf y
{-# INLINE equivUF #-}
countGroupUF :: PrimMonad m => UnionFind (PrimState m) -> m Int
countGroupUF uf = U.length . U.filter (< 0) <$> freezeUnionFind uf
{-# INLINE countGroupUF #-}
-------------------------------------------------------------------------------
-- Geometry
-------------------------------------------------------------------------------
data Point a = P !a !a deriving (Eq, Ord)
instance (Show a) => Show (Point a) where { show (P x y) = shows x $ ' ' : show y}
instance Functor Point where { fmap f (P x y) = P (f x) (f y)}
instance (Num a) => Num (Point a) where { {-# SPECIALISE instance Num (Point Int) #-}; {-# SPECIALISE instance Num (Point Integer) #-}; {-# SPECIALISE instance Num (Point Double) #-}; {-# SPECIALISE instance Num (Point (EPS Double)) #-}; (P x0 y0) + (P x1 y1) = P (x0 + x1) (y0 + y1); (P x0 y0) - (P x1 y1) = P (x0 - x1) (y0 - y1); (P x0 y0) * (P x1 y1) = P (x0 * x1 - y0 * y1) (x0 * y1 + x1 * y0); negate = fmap negate; abs = id; signum _ = P 1 0; fromInteger n = P (fromInteger n) 0}
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
compareCCW o = \ u v -> compare 0 (area o u v)
{-# INLINE compareCCW #-}
compareCW :: (Num a, Ord a) => Point a -> Point a -> Point a -> Ordering
compareCW o = flip (compareCCW o)
{-# INLINE compareCW #-}
sqrNorm :: (Num a) => Point a -> a
sqrNorm v = dot v v
{-# INLINE sqrNorm #-}
norm :: (Floating a) => Point a -> a
norm = sqrt . sqrNorm
{-# INLINE norm #-}
newtype instance  UM.MVector s (Point a) = MV_Point (UM.MVector s a)
newtype instance  U.Vector (Point a) = V_Point (U.Vector a)
instance (U.Unbox a) => U.Unbox (Point a)
instance (U.Unbox a) => GM.MVector UM.MVector (Point a) where { basicLength (MV_Point v) = unsafeShiftR (GM.basicLength v) 1; {-# INLINE basicLength #-}; basicUnsafeSlice i n (MV_Point v) = MV_Point $ GM.basicUnsafeSlice (2 * i) (2 * n) v; {-# INLINE basicUnsafeSlice #-}; basicOverlaps (MV_Point v1) (MV_Point v2) = GM.basicOverlaps v1 v2; {-# INLINE basicOverlaps #-}; basicUnsafeNew n = MV_Point `liftM` GM.basicUnsafeNew (2 * n); {-# INLINE basicUnsafeNew #-}; basicInitialize (MV_Point v) = GM.basicInitialize v; {-# INLINE basicInitialize #-}; basicUnsafeRead (MV_Point v) i = P `liftM` GM.basicUnsafeRead v (2 * i) `ap` GM.basicUnsafeRead v (2 * i + 1); {-# INLINE basicUnsafeRead #-}; basicUnsafeWrite (MV_Point v) i (P x y) = GM.basicUnsafeWrite v (2 * i) x >> GM.basicUnsafeWrite v (2 * i + 1) y; {-# INLINE basicUnsafeWrite #-}; basicClear (MV_Point v) = GM.basicClear v; {-# INLINE basicClear #-}; basicUnsafeCopy (MV_Point v1) (MV_Point v2) = GM.basicUnsafeCopy v1 v2; {-# INLINE basicUnsafeCopy #-}; basicUnsafeMove (MV_Point v1) (MV_Point v2) = GM.basicUnsafeMove v1 v2; {-# INLINE basicUnsafeMove #-}; basicUnsafeGrow (MV_Point v) n = MV_Point `liftM` GM.basicUnsafeGrow v (2 * n); {-# INLINE basicUnsafeGrow #-}}
instance (U.Unbox a) => G.Vector U.Vector (Point a) where { basicUnsafeFreeze (MV_Point v) = V_Point `liftM` G.basicUnsafeFreeze v; {-# INLINE basicUnsafeFreeze #-}; basicUnsafeThaw (V_Point v) = MV_Point `liftM` G.basicUnsafeThaw v; {-# INLINE basicUnsafeThaw #-}; basicLength (V_Point v) = unsafeShiftR (G.basicLength v) 1; {-# INLINE basicLength #-}; basicUnsafeSlice i n (V_Point v) = V_Point $ G.basicUnsafeSlice (2 * i) (2 * n) v; {-# INLINE basicUnsafeSlice #-}; basicUnsafeIndexM (V_Point v) i = P `liftM` G.basicUnsafeIndexM v (2 * i) `ap` G.basicUnsafeIndexM v (2 * i + 1); {-# INLINE basicUnsafeIndexM #-}; basicUnsafeCopy (MV_Point mv) (V_Point v) = G.basicUnsafeCopy mv v; elemseq _ = seq; {-# INLINE elemseq #-}}
-------------------------------------------------------------------------------
-- Data.EPS
-------------------------------------------------------------------------------
eps :: (Fractional a) => a
eps = 1.0e-8
{-# INLINE eps #-}
absErr :: Double -> Double -> Double
absErr ans x = abs (x - ans)
relErr :: Double -> Double -> Double
relErr ans x = abs $ (x - ans) / ans
newtype EPS a = EPS{getEPS :: a} deriving newtype (Show, Read, Num, Fractional, Floating)
instance (Num a, Ord a, Fractional a) => Eq (EPS a) where { {-# SPECIALISE instance Eq (EPS Double) #-}; (EPS x) == (EPS y) = abs (y - x) < eps}
instance (Num a, Ord a, Fractional a) => Ord (EPS a) where { {-# SPECIALISE instance Ord (EPS Double) #-}; compare (EPS x) (EPS y) | abs (x - y) < eps = EQ | otherwise = compare x y; (EPS x) < (EPS y) = x < y - eps; (EPS x) <= (EPS y) = x < y + eps; (EPS x) > (EPS y) = x > y + eps; (EPS x) >= (EPS y) = x > y - eps}
newtype instance  UM.MVector s (EPS a) = MV_EPS (UM.MVector s a)
newtype instance  U.Vector (EPS a) = V_EPS (U.Vector a)
instance (U.Unbox a) => U.Unbox (EPS a)
instance (U.Unbox a) => GM.MVector UM.MVector (EPS a) where { basicLength (MV_EPS v) = GM.basicLength v; {-# INLINE basicLength #-}; basicUnsafeSlice i n (MV_EPS v) = MV_EPS $ GM.basicUnsafeSlice i n v; {-# INLINE basicUnsafeSlice #-}; basicOverlaps (MV_EPS v1) (MV_EPS v2) = GM.basicOverlaps v1 v2; {-# INLINE basicOverlaps #-}; basicUnsafeNew n = MV_EPS `liftM` GM.basicUnsafeNew n; {-# INLINE basicUnsafeNew #-}; basicInitialize (MV_EPS v) = GM.basicInitialize v; {-# INLINE basicInitialize #-}; basicUnsafeReplicate n x = MV_EPS `liftM` GM.basicUnsafeReplicate n (coerce x); {-# INLINE basicUnsafeReplicate #-}; basicUnsafeRead (MV_EPS v) i = coerce `liftM` GM.basicUnsafeRead v i; {-# INLINE basicUnsafeRead #-}; basicUnsafeWrite (MV_EPS v) i x = GM.basicUnsafeWrite v i (coerce x); {-# INLINE basicUnsafeWrite #-}; basicClear (MV_EPS v) = GM.basicClear v; {-# INLINE basicClear #-}; basicSet (MV_EPS v) x = GM.basicSet v (coerce x); {-# INLINE basicSet #-}; basicUnsafeCopy (MV_EPS v1) (MV_EPS v2) = GM.basicUnsafeCopy v1 v2; {-# INLINE basicUnsafeCopy #-}; basicUnsafeMove (MV_EPS v1) (MV_EPS v2) = GM.basicUnsafeMove v1 v2; {-# INLINE basicUnsafeMove #-}; basicUnsafeGrow (MV_EPS v) n = MV_EPS `liftM` GM.basicUnsafeGrow v n; {-# INLINE basicUnsafeGrow #-}}
instance (U.Unbox a) => G.Vector U.Vector (EPS a) where { basicUnsafeFreeze (MV_EPS v) = V_EPS `liftM` G.basicUnsafeFreeze v; {-# INLINE basicUnsafeFreeze #-}; basicUnsafeThaw (V_EPS v) = MV_EPS `liftM` G.basicUnsafeThaw v; {-# INLINE basicUnsafeThaw #-}; basicLength (V_EPS v) = G.basicLength v; {-# INLINE basicLength #-}; basicUnsafeSlice i n (V_EPS v) = V_EPS $ G.basicUnsafeSlice i n v; {-# INLINE basicUnsafeSlice #-}; basicUnsafeIndexM (V_EPS v) i = coerce `liftM` G.basicUnsafeIndexM v i; {-# INLINE basicUnsafeIndexM #-}; basicUnsafeCopy (MV_EPS mv) (V_EPS v) = G.basicUnsafeCopy mv v; elemseq _ = seq; {-# INLINE elemseq #-}}
