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
    [n, l] <- map read.words <$> getLine
    qs <- U.unfoldrN n (runParser $ (,,) <$> int <*> int <*> int) <$> C.getContents
    print $ solve n l qs

solve :: Int -> Int -> U.Vector (Int, Int, Int) -> Int
solve n len (U.modify Intro.sort -> qs) = runST $ do
    hh <- newMinBinaryHeap @(Int, Int) n
    dp <- UM.replicate len (maxBound :: Int)
    let view i = fix $ \loop -> do
            (c, r) <- unsafeViewBH hh
            if r <= i
            then unsafeDeleteBH hh >> loop
            else return $! c

    let go !i ((l,r,c):rest)
            | i < l = do
                view i >>= UM.write dp i
                go (i + 1) ((l,r,c):rest)
            | l <= i = do
                dpl' <- if l > 0 then UM.read dp (l - 1) else pure 0
                insertBH (c + dpl', r) hh
                dpi <- view i
                UM.modify dp (min dpi) i
                go i rest
            | otherwise = undefined
        go i []
            | i < len = do
                view i >>= UM.write dp i
                go (i + 1) []
            | otherwise = pure ()

    go 0 $ U.toList qs

    UM.read dp (len - 1)


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
-- Data.Heap.Binary
-------------------------------------------------------------------------------
data BinaryHeap (f :: * -> *) s a = BinaryHeap{priorityBH :: a -> f a, intVarsBH :: !(UM.MVector s Int), internalVecBH :: !(UM.MVector s a)}
_sizeBH :: Int
_sizeBH = 0
{-# INLINE _sizeBH #-}
type MinBinaryHeap s a = BinaryHeap Identity s a
type MaxBinaryHeap s a = BinaryHeap Down s a
newBinaryHeap :: (U.Unbox a, PrimMonad m) => (a -> f a) -> Int -> m (BinaryHeap f (PrimState m) a)
newBinaryHeap prio n = BinaryHeap prio <$> UM.replicate 1 0 <*> UM.unsafeNew n
newMinBinaryHeap :: (U.Unbox a, PrimMonad m) => Int -> m (MinBinaryHeap (PrimState m) a)
newMinBinaryHeap = newBinaryHeap Identity
newMaxBinaryHeap :: (U.Unbox a, PrimMonad m) => Int -> m (MaxBinaryHeap (PrimState m) a)
newMaxBinaryHeap = newBinaryHeap Down
getBinaryHeapSize :: (PrimMonad m) => BinaryHeap f (PrimState m) a -> m Int
getBinaryHeapSize BinaryHeap{..} = UM.unsafeRead intVarsBH _sizeBH
{-# INLINE getBinaryHeapSize #-}
siftUpBy :: (U.Unbox a, PrimMonad m) => (a -> a -> Ordering) -> Int -> UM.MVector (PrimState m) a -> m ()
siftUpBy cmp k vec = do { x <- UM.unsafeRead vec k; flip fix k $ \ loop !i -> if i > 0 then do { let { parent = (i - 1) `unsafeShiftR` 1}; p <- UM.unsafeRead vec parent; case cmp p x of { GT -> UM.unsafeWrite vec i p >> loop parent; _ -> UM.unsafeWrite vec i x}} else UM.unsafeWrite vec 0 x}
{-# INLINE siftUpBy #-}
siftDownBy :: (U.Unbox a, PrimMonad m) => (a -> a -> Ordering) -> Int -> UM.MVector (PrimState m) a -> m ()
siftDownBy cmp k vec = do { x <- UM.unsafeRead vec k; let { !n = UM.length vec}; flip fix k $ \ loop !i -> do { let { l = unsafeShiftL i 1 .|. 1}; let { r = l + 1}; if n <= l then UM.unsafeWrite vec i x else do { vl <- UM.unsafeRead vec l; if r < n then do { vr <- UM.unsafeRead vec r; case cmp vr vl of { LT -> case cmp x vr of { GT -> UM.unsafeWrite vec i vr >> loop r; _ -> UM.unsafeWrite vec i x}; _ -> case cmp x vl of { GT -> UM.unsafeWrite vec i vl >> loop l; _ -> UM.unsafeWrite vec i x}}} else case cmp x vl of { GT -> UM.unsafeWrite vec i vl >> loop l; _ -> UM.unsafeWrite vec i x}}}}
{-# INLINE siftDownBy #-}
heapifyBy :: (U.Unbox a, PrimMonad m) => (a -> a -> Ordering) -> UM.MVector (PrimState m) a -> m ()
heapifyBy cmp vec = do { rev (UM.length vec `quot` 2) $ \ i -> do { siftDownBy cmp i vec}}
{-# INLINE heapifyBy #-}
class OrdVia f a where { compareVia :: (a -> f a) -> a -> a -> Ordering}
instance (Ord a) => OrdVia Identity a where { compareVia _ = coerce (compare :: Identity a -> Identity a -> Ordering); {-# INLINE compareVia #-}}
instance (Ord a) => OrdVia Down a where { compareVia _ = coerce (compare :: Down a -> Down a -> Ordering); {-# INLINE compareVia #-}}
buildBinaryHeapVia :: (OrdVia f a, U.Unbox a, PrimMonad m) => (a -> f a) -> U.Vector a -> m (BinaryHeap f (PrimState m) a)
buildBinaryHeapVia ~priorityBH vec = do { intVarsBH <- UM.replicate 1 $ U.length vec; internalVecBH <- U.thaw vec; heapifyBy (compareVia priorityBH) internalVecBH; return $! BinaryHeap{..}}
{-# INLINE buildBinaryHeapVia #-}
buildMinBinaryHeap :: (Ord a, U.Unbox a, PrimMonad m) => U.Vector a -> m (BinaryHeap Identity (PrimState m) a)
buildMinBinaryHeap = buildBinaryHeapVia Identity
{-# INLINE buildMinBinaryHeap #-}
buildMaxBinaryHeap :: (Ord a, U.Unbox a, PrimMonad m) => U.Vector a -> m (BinaryHeap Down (PrimState m) a)
buildMaxBinaryHeap = buildBinaryHeapVia Down
{-# INLINE buildMaxBinaryHeap #-}
unsafeViewBH :: (U.Unbox a, PrimMonad m) => BinaryHeap f (PrimState m) a -> m a
unsafeViewBH BinaryHeap{..} = UM.unsafeRead internalVecBH 0
{-# INLINE unsafeViewBH #-}
viewBH :: (U.Unbox a, PrimMonad m) => BinaryHeap f (PrimState m) a -> m (Maybe a)
viewBH bh = do { size <- getBinaryHeapSize bh; if size > 0 then Just <$!> unsafeViewBH bh else return $! Nothing}
{-# INLINE viewBH #-}
insertBH :: (OrdVia f a, U.Unbox a, PrimMonad m) => a -> BinaryHeap f (PrimState m) a -> m ()
insertBH x BinaryHeap{..} = do { size <- UM.unsafeRead intVarsBH _sizeBH; UM.unsafeWrite intVarsBH _sizeBH (size + 1); UM.unsafeWrite internalVecBH size x; siftUpBy (compareVia priorityBH) size internalVecBH}
{-# INLINE insertBH #-}
unsafeDeleteBH :: (OrdVia f a, U.Unbox a, PrimMonad m) => BinaryHeap f (PrimState m) a -> m ()
unsafeDeleteBH BinaryHeap{..} = do { size' <- subtract 1 <$!> UM.unsafeRead intVarsBH _sizeBH; UM.unsafeWrite intVarsBH _sizeBH size'; UM.unsafeSwap internalVecBH 0 size'; siftDownBy (compareVia priorityBH) 0 (UM.unsafeTake size' internalVecBH)}
{-# INLINE unsafeDeleteBH #-}
modifyTopBH :: (OrdVia f a, U.Unbox a, PrimMonad m) => (a -> a) -> BinaryHeap f (PrimState m) a -> m ()
modifyTopBH f BinaryHeap{..} = do { UM.unsafeModify internalVecBH f 0; size <- UM.unsafeRead intVarsBH _sizeBH; siftDownBy (compareVia priorityBH) 0 (UM.unsafeTake size internalVecBH)}
{-# INLINE modifyTopBH #-}
deleteFindTopBH :: (Ord a, U.Unbox a, PrimMonad m) => MinBinaryHeap (PrimState m) a -> m (Maybe a)
deleteFindTopBH bh = do { size <- getBinaryHeapSize bh; if size > 0 then do { !top <- unsafeViewBH bh <* unsafeDeleteBH bh; return $ Just top} else return Nothing}
{-# INLINE deleteFindTopBH #-}
clearBH :: (PrimMonad m) => BinaryHeap f (PrimState m) a -> m ()
clearBH BinaryHeap{..} = UM.unsafeWrite intVarsBH 0 0
freezeInternalVecBH :: (U.Unbox a, PrimMonad m) => BinaryHeap f (PrimState m) a -> m (U.Vector a)
freezeInternalVecBH BinaryHeap{..} = do { size <- UM.unsafeRead intVarsBH _sizeBH; U.unsafeFreeze (UM.unsafeTake size internalVecBH)}
