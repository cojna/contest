{-# OPTIONS_GHC -O2 -funbox-strict-fields #-}
{-# LANGUAGE BangPatterns #-}

import           Control.Applicative
import           Control.Monad
import           Control.Monad.ST
import           Data.Bits
import qualified Data.ByteString.Char8       as B
import           Data.Char
import           Data.Function
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed         as U
import           Data.Word
import           Unsafe.Coerce

main :: IO ()
main = do
    n <- readLn :: IO Int
    xs <- U.unfoldrN n (B.readInt.B.dropWhile isSpace) <$> B.getLine
    putStrLn.unwords.map show.U.toList $ introSort xs

-------------------------------------------------------------------------------

rep, rev :: Monad m => Int -> (Int -> m ()) -> m ()
rep !n f = U.mapM_ f $ U.generate n id
rev !n f = U.mapM_ f $ U.iterateN n (subtract 1) (n - 1)
for :: Monad m => Int -> Int -> (Int -> m ()) -> m ()
for !s !t f = U.mapM_ f $ U.generate (t - s) (+s)
{-# INLINE rep #-}
{-# INLINE rev #-}
{-# INLINE for #-}

introSort :: (Ord a, G.Vector v a) => v a -> v a
introSort = introSortBy compare

introSortBy :: (G.Vector v a) => (a -> a -> Ordering) -> v a -> v a
introSortBy cmp = G.modify $ inplaceIntroSortBy cmp

inplaceIntroSortBy :: (GM.MVector mv a)
                   => (a -> a -> Ordering) -> mv s a -> ST s ()
inplaceIntroSortBy cmp vec = do
    let depthLimit = 2 * floorLog2 (GM.length vec)
        threshold = 16
    fix `flip` depthLimit `flip` vec $ \loop !depth mv ->
        when (GM.length mv > threshold) $
            if depth > 0
            then do
                pivot <- getMedian3Pivot cmp mv
                cut <- pivotPartition cmp mv pivot
                loop (depth - 1) (GM.unsafeDrop cut mv)
                loop (depth - 1) (GM.unsafeTake cut mv)
            else inplaceHeapSortBy cmp mv
    inplaceInsertionSortBy cmp vec
  where
    floorLog2 :: Int -> Int
    floorLog2 x = fromIntegral $ unsafeShiftR y 52 - 1023
      where
        y :: Word64
        y = unsafeCoerce (fromIntegral x :: Double)

insertionSort :: (Ord a, G.Vector v a) => v a -> v a
insertionSort = insertionSortBy compare
{-# INLINE insertionSort #-}

insertionSortBy :: (G.Vector v a) => (a -> a -> Ordering) -> v a -> v a
insertionSortBy cmp = G.modify $ inplaceInsertionSortBy cmp
{-# INLINE insertionSortBy #-}

inplaceInsertionSortBy :: (GM.MVector mv a)
                       => (a -> a -> Ordering) -> mv s a -> ST s ()
inplaceInsertionSortBy cmp vec =
    for 1 (GM.length vec) $ \i -> do
        x <- GM.unsafeRead vec i
        hd <- GM.unsafeRead vec 0
        case cmp hd x of
            LT -> flip fix i $ \loop !j -> do
                y <- GM.unsafeRead vec (j - 1)
                case cmp x y of
                    LT -> do
                        GM.unsafeWrite vec j y
                        loop (j - 1)
                    _ -> GM.unsafeWrite vec j x
            _ -> flip fix i $ \loop !j ->
                if j > 0
                then do
                    GM.unsafeRead vec (j - 1) >>= GM.unsafeWrite vec j
                    loop (j - 1)
                else GM.unsafeWrite vec 0 x
{-# INLINE inplaceInsertionSortBy #-}

quickSort :: (Ord a, G.Vector v a) => v a -> v a
quickSort = quickSortBy compare
{-# INLINE quickSort #-}

quickSortBy :: (G.Vector v a) => (a -> a -> Ordering) -> v a -> v a
quickSortBy cmp = G.modify $ fix $ \loop vec ->
    when (GM.length vec > 1) $ do
        pivot <- getMedian3Pivot cmp vec
        cut <- pivotPartition cmp vec pivot
        loop (GM.unsafeDrop cut vec)
        loop (GM.unsafeTake cut vec)
{-# INLINE quickSortBy #-}

pivotPartition :: (GM.MVector mv a)
               => (a -> a -> Ordering) -> mv s a -> a -> ST s Int
pivotPartition cmp vec pivot =
    fix `flip` 0 `flip` GM.length vec $ \loop !l !r -> do
        !l' <- flip fix l $ \loopL !i -> do
            x <- GM.unsafeRead vec i
            case cmp x pivot of
                LT -> loopL (i + 1)
                _ -> return i
        !r' <- flip fix (r - 1) $ \loopR !i -> do
            x <- GM.unsafeRead vec i
            case cmp pivot x of
                LT -> loopR (i - 1)
                _ -> return i
        if l' < r'
        then do
            GM.unsafeSwap vec l' r'
            loop (l' + 1) r'
        else return l'
{-# INLINE pivotPartition #-}

getMedian3Pivot :: (GM.MVector mv a)
                => (a -> a -> Ordering) -> mv s a -> ST s a
getMedian3Pivot cmp vec = median cmp
    <$> GM.unsafeRead vec 0
    <*> GM.unsafeRead vec (GM.length vec `quot` 2)
    <*> GM.unsafeRead vec (GM.length vec - 1)
{-# INLINE getMedian3Pivot #-}

median :: (a -> a -> Ordering) -> a -> a -> a -> a
median cmp x y z = case cmp x y of
    LT -> case cmp y z of
        LT -> y
        _ -> case cmp x z of
            LT -> z
            _ -> x
    _ -> case cmp x z of
        LT -> x
        _ -> case cmp y z of
            LT -> z
            _ -> y
{-# INLINE median #-}

heapSort :: (Ord a, G.Vector v a) => v a -> v a
heapSort = heapSortBy compare
{-# INLINE heapSort #-}

heapSortBy :: (G.Vector v a) => (a -> a -> Ordering) -> v a -> v a
heapSortBy cmp = G.modify $ inplaceHeapSortBy cmp
{-# INLINE heapSortBy #-}

inplaceHeapSortBy :: (GM.MVector mv a)
                  => (a -> a -> Ordering) -> mv s a -> ST s ()
inplaceHeapSortBy cmp vec = do
    heapify cmp vec
    flip fix (GM.length vec - 1) $ \loop !i ->
        when (i > 0) $ do
            GM.unsafeSwap vec 0 i
            siftDown cmp 0 $ GM.unsafeTake i vec
            loop (i - 1)
{-# INLINE inplaceHeapSortBy #-}

heapify :: (GM.MVector mv a)
        => (a -> a -> Ordering) -> mv s a -> ST s ()
heapify cmp vec =
    rev (GM.length vec `quot` 2) $ \i ->
        siftDown cmp i vec
{-# INLINE heapify #-}

siftDown :: (GM.MVector mv a)
         => (a -> a -> Ordering) -> Int -> mv s a -> ST s ()
siftDown cmp offset vec = do
    let !len = GM.length vec
    flip fix offset $ \loop !parent -> do
        let !l = 2 * parent + 1
            !r = l + 1
        x <- GM.unsafeRead vec parent
        when (l < len) $ do
            childL <- GM.unsafeRead vec l
            if r < len
            then do
                childR <- GM.unsafeRead vec r
                case cmp childL childR of
                    LT -> when (cmp x childR == LT) $ do
                        GM.unsafeSwap vec parent r
                        loop r
                    _ -> when (cmp x childL == LT) $ do
                        GM.unsafeSwap vec parent l
                        loop l
            else when (cmp x childL == LT) $ do
                GM.unsafeSwap vec parent l
                loop l
{-# INLINE siftDown #-}