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
    [h, w] <- map read.words <$> getLine
    mat <- U.unfoldrN (h * w) (runParser char)
        . C.filter (not.isSpace) <$> C.getContents
    print $ solve h w mat

solve :: Int -> Int -> U.Vector Char -> Int
solve h w mat = runST $ do
    uf <- newUnionFind (h * w + 1)
    flip U.imapM_ mat $ \xy -> \case
        '#' -> uniteUF_ uf xy wall
        _ -> do
            let (x, y) = quotRem xy w
            neighbor4 h w x y $ \nx ny -> do
                when (mat U.! ix nx ny == '.') $ do
                    uniteUF_ uf xy (ix nx ny)

    res <- UM.replicate (h * w) False
    countGroupUF uf >>= \case
        2 -> do
            rep h $ \x -> do
                rep w $ \y -> when (mat U.! ix x y == '#') $ do
                    neighbor4 h w x y $ \nx ny -> when (mat U.! ix nx ny == '.') $ do
                        UM.write res (ix x y) True
        numG -> do
            rep h $ \x -> do
                rep w $ \y -> when (mat U.! ix x y == '#') $ do
                    set <- newMutVar IS.empty
                    neighbor4 h w x y $ \nx ny -> when (mat U.! ix nx ny == '.') $ do
                        repr <- findUF uf (ix nx ny)
                        modifyMutVar' set (IS.insert repr)
                    size <- IS.size <$> readMutVar set
                    when (size == numG - 1) $ do
                        UM.write res (ix x y) True
    U.length . U.filter id  <$> U.unsafeFreeze res
  where
    ix x y = x * w + y
    {-# INLINE ix #-}
    wall = h * w

neighbor4 :: (Applicative f) => Int -> Int -> Int -> Int -> (Int -> Int -> f ()) -> f ()
neighbor4 h w x y f
    =  when (x > 0) (f (x - 1) y)
    *> when (y > 0) (f x (y - 1))
    *> when (y < w - 1) (f x (y + 1))
    *> when (x < h - 1) (f (x + 1) y)
{-# INLINE neighbor4 #-}


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
