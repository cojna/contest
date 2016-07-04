{-# OPTIONS_GHC -O2 -funbox-strict-fields #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Control.Monad.State.Strict
import           Data.Bool
import qualified Data.ByteString.Char8       as B
import qualified Data.ByteString.Unsafe      as B
import           Data.Char
import           Data.Function
import qualified Data.IntMap.Strict          as IM
import qualified Data.IntSet                 as IS
import           Data.List
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
import           Unsafe.Coerce

main :: IO ()
main = do
    [n, m, s] <- map read.words <$> getLine :: IO [Int]
    uvs <- U.unfoldrN m parseInt2 <$> B.getContents
    putStr.unlines.map show $ solve n s uvs

solve :: Int -> Int -> U.Vector (Int, Int) -> [Int]
solve n s uvs = runST $ do
    uf <- newUnionFind n 
    let go res iuvs (i:is) = do
            let (xs, ys) = U.span (\(j,_,_)->j >= i) iuvs
            U.forM_ xs $ \(_,u,v) -> do
                uniteM u v uf
            ok <- equivM i (s-1) uf
            go (bool res (i:res) ok) ys is
        go res _ [] = return $ map (+1) res
    let (xs, ys) = U.span (\(x,_,_)->x >= s - 1) iuvs0
    U.forM_ xs $ \(_,u,v) -> do
        uniteM u v uf
    go [] ys [s-1,s-2..0]
  where
    !iuvs0 = U.reverse . introSort $ U.map (\(u,v) -> (min u v - 1, u - 1, v - 1)) uvs

-------------------------------------------------------------------------------
rep, rev :: Monad m => Int -> (Int -> m ()) -> m ()
rep !n = U.forM_ $ U.generate n id
rev !n = U.forM_ $ U.iterateN n (subtract 1) (n - 1)
for :: Monad m => Int -> Int -> (Int -> m ()) -> m ()
for !s !t = U.forM_ $ U.generate (t - s) (+s)
{-# INLINE rep #-}
{-# INLINE rev #-}
{-# INLINE for #-}

type Parser a = B.ByteString -> Maybe (a, B.ByteString)

parseInt :: Parser Int
parseInt = B.readInt . B.dropWhile isSpace

parseInt2 :: Parser (Int, Int)
parseInt2 = runStateT $
    (,) <$> StateT (B.readInt . B.dropWhile isSpace)
        <*> StateT (B.readInt . B.unsafeTail)

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
            GT -> flip fix i $ \loop !j ->
                if j > 0
                then do
                    GM.unsafeRead vec (j - 1) >>= GM.unsafeWrite vec j
                    loop (j - 1)
                else GM.unsafeWrite vec 0 x
            _ -> flip fix i $ \loop !j -> do
                y <- GM.unsafeRead vec (j - 1)
                case cmp x y of
                    LT -> do
                        GM.unsafeWrite vec j y
                        loop (j - 1)
                    _ -> GM.unsafeWrite vec j x
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


data UnionFind m = UF
    { parent :: UM.MVector m Int
    , rank   :: UM.MVector m Int
    }

nothing :: Int
nothing = -1
{-# INLINE nothing #-}

newUnionFind :: PrimMonad m => Int -> m (UnionFind (PrimState m))
newUnionFind n = UF `liftM` UM.replicate n nothing `ap` UM.replicate n 0
{-# INLINE newUnionFind #-}

findM :: PrimMonad m => Int -> UnionFind (PrimState m) -> m Int
findM x uf@UF{..} = do
    px <- UM.unsafeRead parent x
    if px == nothing
    then return x
    else do
        ppx <- findM px uf
        UM.unsafeWrite parent x ppx
        return ppx
{-# INLINE findM #-}

uniteM :: PrimMonad m => Int -> Int -> UnionFind (PrimState m) -> m ()
uniteM x y uf@UF{..} = do
    px <- findM x uf
    py <- findM y uf
    when (px /= py) $ do
        rx <- UM.unsafeRead rank px
        ry <- UM.unsafeRead rank py
        case compare rx ry of
            LT -> UM.unsafeWrite parent px py
            GT -> UM.unsafeWrite parent py px
            EQ -> do
                UM.unsafeWrite parent px py
                UM.unsafeWrite rank py (ry + 1)
{-# INLINE uniteM #-}

equivM :: PrimMonad m => Int -> Int -> UnionFind (PrimState m) -> m Bool
equivM x y uf = (==) `liftM` findM x uf `ap` findM y uf
{-# INLINE equivM #-}

countM :: PrimMonad m => UnionFind (PrimState m) -> m Int
countM UF{..} = liftM (U.length . U.filter (==nothing)) $ U.unsafeFreeze parent
{-# INLINE countM #-}
