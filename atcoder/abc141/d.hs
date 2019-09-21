{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

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
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM
import           Debug.Trace
import           Foreign                     hiding (void)
import           GHC.Exts
import qualified System.IO                   as IO
import           Unsafe.Coerce

main :: IO ()
main = do
    [n,m] <- map read.words <$> getLine :: IO [Int]
    xs <- U.unfoldrN n parseInt <$> C.getLine
    print $ solve n m xs

solve :: Int -> Int -> U.Vector Int -> Int
solve n m xs = runST $ do
    h <- buildBinaryHeap xs
    rep m $ \_ -> do
        modifyMaxBH h (`quot` 2)
    vec <- freezeInternalBinaryHeapBH h
    return $ U.sum vec

-------------------------------------------------------------------------------
data BinaryHeap s a = BinaryHeap (MutVar s Int) (UM.MVector s a)

newBinaryHeap :: (PrimMonad m, U.Unbox a) => Int -> m (BinaryHeap (PrimState m) a)
newBinaryHeap n = BinaryHeap <$> newMutVar 0 <*> UM.new n

getBinaryHeapSize :: (PrimMonad m) => BinaryHeap (PrimState m) a -> m Int
getBinaryHeapSize (BinaryHeap ref _) = readMutVar ref
{-# INLINE getBinaryHeapSize #-}

siftUp :: (PrimMonad m, U.Unbox a, Ord a) => UM.MVector (PrimState m) a -> Int -> m ()
siftUp vec k = do
    x <- UM.unsafeRead vec k
    flip fix k $ \loop !i ->
        if i > 0
        then do
            let !parent = (i - 1) `unsafeShiftR` 1
            p <- UM.unsafeRead vec parent
            if p >= x
            then UM.unsafeWrite vec i x
            else do
                UM.unsafeWrite vec i p
                loop parent
        else UM.unsafeWrite vec 0 x
{-# INLINE siftUp #-}

siftDown :: (PrimMonad m, U.Unbox a, Ord a) => UM.MVector (PrimState m) a -> Int -> m ()
siftDown vec k = do
    x <- UM.unsafeRead vec k
    let n = UM.length vec
    flip fix k $ \loop !i -> do
        let !l = unsafeShiftL i 1 .|. 1
        if n <= l
        then UM.unsafeWrite vec i x
        else do
            let !r = l + 1
            childL <- UM.unsafeRead vec l
            childR <- UM.unsafeRead vec r
            if r < n && childR > childL
            then if x >= childR
                 then UM.unsafeWrite vec i x
                 else do
                     UM.unsafeWrite vec i childR
                     loop r
            else if x >= childL
                 then UM.unsafeWrite vec i x
                 else do
                     UM.unsafeWrite vec i childL
                     loop l
{-# INLINE siftDown #-}

heapify :: (PrimMonad m, U.Unbox a, Ord a) => UM.MVector (PrimState m) a -> m ()
heapify vec = do
    rev (UM.length vec `quot` 2) $ \i -> do
        siftDown vec i
{-# INLINE heapify #-}

buildBinaryHeap :: (PrimMonad m, U.Unbox a, Ord a)
    => U.Vector a -> m (BinaryHeap (PrimState m) a)
buildBinaryHeap vec = do
    ref <- newMutVar $ U.length vec
    mvec <- U.unsafeThaw vec
    heapify mvec
    return $! BinaryHeap ref mvec
{-# INLINE buildBinaryHeap #-}

unsafeMaxViewBH :: (PrimMonad m, U.Unbox a) => BinaryHeap (PrimState m) a -> m a
unsafeMaxViewBH (BinaryHeap _ vec) = UM.unsafeRead vec 0
{-# INLINE unsafeMaxViewBH #-}

maxViewBH :: (PrimMonad m, U.Unbox a)
    => BinaryHeap (PrimState m) a -> m (Maybe a)
maxViewBH bh = do
    size <- getBinaryHeapSize bh
    if size > 0
    then Just <$!> unsafeMaxViewBH bh
    else return $! Nothing
{-# INLINE maxViewBH #-}

insertMaxBH :: (PrimMonad m, U.Unbox a, Ord a)
    => a -> BinaryHeap (PrimState m) a -> m ()
insertMaxBH x bh@(BinaryHeap info vec) = do
    size <- getBinaryHeapSize bh
    modifyMutVar' info (+1)
    UM.unsafeWrite vec size x
    siftUp vec size
{-# INLINE insertMaxBH #-}

unsafeDeleteMaxBH :: (PrimMonad m, U.Unbox a, Ord a)
    => BinaryHeap (PrimState m) a -> m ()
unsafeDeleteMaxBH bh@(BinaryHeap info vec) = do
    size <- getBinaryHeapSize bh
    modifyMutVar' info (subtract 1)
    UM.unsafeSwap vec 0 (size - 1)
    siftDown (UM.unsafeTake (size - 1) vec) 0
{-# INLINE unsafeDeleteMaxBH #-}

modifyMaxBH :: (PrimMonad m, U.Unbox a, Ord a)
    => BinaryHeap (PrimState m) a -> (a -> a) -> m ()
modifyMaxBH bh@(BinaryHeap _ vec) f = do
    UM.unsafeModify vec f 0
    size <- getBinaryHeapSize bh
    siftDown (UM.unsafeTake size vec) 0
{-# INLINE modifyMaxBH #-}

deleteFindMaxBH :: (PrimMonad m, U.Unbox a, Ord a)
    => BinaryHeap (PrimState m) a -> m (Maybe a)
deleteFindMaxBH bh@(BinaryHeap _ vec) = do
    size <- getBinaryHeapSize bh
    if size > 0
    then Just <$!> unsafeMaxViewBH bh <* unsafeDeleteMaxBH bh
    else return $! Nothing
{-# INLINE deleteFindMaxBH #-}

clearBH :: (PrimMonad m) => BinaryHeap (PrimState m) a -> m ()
clearBH (BinaryHeap info _) = writeMutVar info 0

freezeInternalBinaryHeapBH :: (PrimMonad m, U.Unbox a)
    => BinaryHeap (PrimState m) a -> m (U.Vector a)
freezeInternalBinaryHeapBH bh@(BinaryHeap _ vec) = do
    size <- getBinaryHeapSize bh
    U.unsafeFreeze (UM.unsafeTake size vec)

rep, rev :: Monad m => Int -> (Int -> m ()) -> m ()
rep !n = U.forM_ $ U.generate n id
rev !n = U.forM_ $ U.iterateN n (subtract 1) (n - 1)
{-# INLINE rep #-}
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