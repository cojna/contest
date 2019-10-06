{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns, CPP, LambdaCase, MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses, MultiWayIf, OverloadedStrings #-}
{-# LANGUAGE TupleSections, TypeFamilies, ViewPatterns            #-}

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Control.Monad.State.Strict
import           Data.Bool
import qualified Data.ByteString             as B
import qualified Data.ByteString.Builder     as B
import qualified Data.ByteString.Char8       as C
import qualified Data.ByteString.Internal    as B
import qualified Data.ByteString.Unsafe      as B
import           Data.Char
import qualified Data.Foldable               as F
import           Data.Function
import qualified Data.IntMap.Strict          as IM
import qualified Data.IntSet                 as IS
import qualified Data.List                   as L
import qualified Data.Map.Strict             as M
import           Data.Monoid
import           Data.Ord
import           Data.Primitive.MutVar
import qualified Data.Set                    as S
import           Data.Tuple
import qualified Data.Vector                 as V
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Mutable         as VM
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM
import           Debug.Trace
import           Foreign                     hiding (void)
import           GHC.Exts
import qualified System.IO                   as IO
import           Unsafe.Coerce

main :: IO ()
main = do
    n <- readLn :: IO Int
    xs <- U.unfoldrN (3 * n) parseInt <$> C.getLine
    print $ solve n xs

solve :: Int -> U.Vector Int -> Int
solve n xs = U.maximum $ U.zipWith (+) (U.drop n front) (U.take (n + 1) rear)
  where
    front = topKSum n $ U.take (2 * n) xs
    rear = U.reverse . topKSum n . U.reverse . U.map negate $ U.drop n xs

topKSum :: Int -> U.Vector Int -> U.Vector Int
topKSum k xs = runST $ do
    heap <- buildBinaryHeap xs0
    res <- UM.replicate (n + 1) 0
    flip U.imapM xs0 $ \i x -> do
        UM.unsafeRead res i >>= UM.unsafeWrite res (i + 1) . (+x)

    flip U.imapM xs' $ \i0 x -> do
        let i = i0 + k
        top <- unsafeMinViewBH heap
        if top < x
        then do
            modifyMinBH heap (const x)
            UM.unsafeRead res i >>= UM.unsafeWrite res (i + 1) . (+(x-top))
        else UM.unsafeRead res i >>= UM.unsafeWrite res (i + 1)
    U.unsafeFreeze res
  where
    n = U.length xs
    (xs0, xs') = U.splitAt k xs

-------------------------------------------------------------------------------
rep :: Monad m => Int -> (Int -> m ()) -> m ()
rep !n = U.forM_ $ U.generate n id
{-# INLINE rep #-}

rev :: Monad m => Int -> (Int -> m ()) -> m ()
rev !n = U.forM_ $ U.iterateN n (subtract 1) (n - 1)
{-# INLINE rev #-}

type Parser a = C.ByteString -> Maybe (a, C.ByteString)

parseInt :: Parser Int
parseInt = C.readInt . C.dropWhile isSpace

parseInt2 :: Parser (Int, Int)
parseInt2 = runStateT $
    (,) <$> StateT (C.readInt . C.dropWhile isSpace)
        <*> StateT (C.readInt . B.unsafeTail)

parseInt3 :: Parser (Int, Int, Int)
parseInt3 = runStateT $
    (,,) <$> StateT (C.readInt . C.dropWhile isSpace)
        <*> StateT (C.readInt . B.unsafeTail)
        <*> StateT (C.readInt . B.unsafeTail)

parseInt4 :: Parser (Int, Int, Int, Int)
parseInt4 = runStateT $
    (,,,) <$> StateT (C.readInt . C.dropWhile isSpace)
        <*> StateT (C.readInt . B.unsafeTail)
        <*> StateT (C.readInt . B.unsafeTail)
        <*> StateT (C.readInt . B.unsafeTail)
-------------------------------------------------------------------------------
-- Data.Heap.BinaryHeap.Min
-------------------------------------------------------------------------------
data BinaryHeap s a = BinaryHeap (MutVar s Int) (UM.MVector s a)
newBinaryHeap :: (PrimMonad m, U.Unbox a) => Int -> m (BinaryHeap (PrimState m) a)
newBinaryHeap n = BinaryHeap <$> newMutVar 0 <*> UM.new n
getBinaryHeapSize :: (PrimMonad m) => BinaryHeap (PrimState m) a -> m Int
getBinaryHeapSize (BinaryHeap ref _) = readMutVar ref
{-# INLINE getBinaryHeapSize #-}
siftUp :: (PrimMonad m, U.Unbox a, Ord a) => Int -> UM.MVector (PrimState m) a -> m ()
siftUp k vec = do { x <- UM.unsafeRead vec k; flip fix k $ \ loop !i -> if i > 0 then do { let { !parent = (i - 1) `unsafeShiftR` 1}; p <- UM.unsafeRead vec parent; if p <= x then UM.unsafeWrite vec i x else do { UM.unsafeWrite vec i p; loop parent}} else UM.unsafeWrite vec 0 x}
{-# INLINE siftUp #-}
siftDown :: (PrimMonad m, U.Unbox a, Ord a) => Int -> UM.MVector (PrimState m) a -> m ()
siftDown k vec = do { x <- UM.unsafeRead vec k; let { n = UM.length vec}; flip fix k $ \ loop !i -> do { let { !l = unsafeShiftL i 1 .|. 1}; if n <= l then UM.unsafeWrite vec i x else do { let { !r = l + 1}; childL <- UM.unsafeRead vec l; childR <- UM.unsafeRead vec r; if r < n && childR < childL then if x <= childR then UM.unsafeWrite vec i x else do { UM.unsafeWrite vec i childR; loop r} else if x <= childL then UM.unsafeWrite vec i x else do { UM.unsafeWrite vec i childL; loop l}}}}
{-# INLINE siftDown #-}
heapify :: (PrimMonad m, U.Unbox a, Ord a) => UM.MVector (PrimState m) a -> m ()
heapify vec = do { rev (UM.length vec `quot` 2) $ \ i -> do { siftDown i vec}}
{-# INLINE heapify #-}
buildBinaryHeap :: (PrimMonad m, U.Unbox a, Ord a) => U.Vector a -> m (BinaryHeap (PrimState m) a)
buildBinaryHeap vec = do { ref <- newMutVar $ U.length vec; mvec <- U.unsafeThaw vec; heapify mvec; return $! BinaryHeap ref mvec}
{-# INLINE buildBinaryHeap #-}
unsafeMinViewBH :: (PrimMonad m, U.Unbox a) => BinaryHeap (PrimState m) a -> m a
unsafeMinViewBH (BinaryHeap _ vec) = UM.unsafeRead vec 0
{-# INLINE unsafeMinViewBH #-}
minViewBH :: (PrimMonad m, U.Unbox a) => BinaryHeap (PrimState m) a -> m (Maybe a)
minViewBH bh = do { size <- getBinaryHeapSize bh; if size > 0 then Just <$!> unsafeMinViewBH bh else return $! Nothing}
{-# INLINE minViewBH #-}
insertMinBH :: (PrimMonad m, U.Unbox a, Ord a) => a -> BinaryHeap (PrimState m) a -> m ()
insertMinBH x bh@(BinaryHeap info vec) = do { size <- getBinaryHeapSize bh; modifyMutVar' info (+ 1); UM.unsafeWrite vec size x; siftUp size vec}
{-# INLINE insertMinBH #-}
unsafeDeleteMinBH :: (PrimMonad m, U.Unbox a, Ord a) => BinaryHeap (PrimState m) a -> m ()
unsafeDeleteMinBH bh@(BinaryHeap info vec) = do { size <- getBinaryHeapSize bh; modifyMutVar' info (subtract 1); UM.unsafeSwap vec 0 (size - 1); siftDown 0 (UM.unsafeTake (size - 1) vec)}
{-# INLINE unsafeDeleteMinBH #-}
modifyMinBH :: (PrimMonad m, U.Unbox a, Ord a) => BinaryHeap (PrimState m) a -> (a -> a) -> m ()
modifyMinBH bh@(BinaryHeap _ vec) f = do { UM.unsafeModify vec f 0; size <- getBinaryHeapSize bh; siftDown 0 (UM.unsafeTake size vec)}
{-# INLINE modifyMinBH #-}
deleteFindMinBH :: (PrimMonad m, U.Unbox a, Ord a) => BinaryHeap (PrimState m) a -> m (Maybe a)
deleteFindMinBH bh@(BinaryHeap _ vec) = do { size <- getBinaryHeapSize bh; if size > 0 then Just <$!> unsafeMinViewBH bh <* unsafeDeleteMinBH bh else return $! Nothing}
{-# INLINE deleteFindMinBH #-}
clearBH :: (PrimMonad m) => BinaryHeap (PrimState m) a -> m ()
clearBH (BinaryHeap info _) = writeMutVar info 0
freezeInternalBinaryHeapBH :: (PrimMonad m, U.Unbox a) => BinaryHeap (PrimState m) a -> m (U.Vector a)
freezeInternalBinaryHeapBH bh@(BinaryHeap _ vec) = do { size <- getBinaryHeapSize bh; U.unsafeFreeze (UM.unsafeTake size vec)}