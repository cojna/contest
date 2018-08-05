{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}

import           Control.Monad
import           Control.Monad.Primitive
import           Data.Bits
import qualified Data.ByteString.Char8       as B
import qualified Data.ByteString.Unsafe      as B
import qualified Data.Foldable               as F
import           Data.Function
import           Data.Primitive.MutVar
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM
import           Data.Word

import           Debug.Trace


main :: IO ()
main = do
  [n, m, k] <- map read.words <$> getLine
  dirs <- B.getLine
  bs <- B.concat.B.lines <$> B.getContents
  case solve n m k dirs bs of
      Nothing  -> print (-1)
      Just res -> print res

solve :: Int -> Int -> Int -> B.ByteString -> B.ByteString -> Maybe Int
solve n m k dirs bs
    | dg == inf = Nothing
    | otherwise = Just dg
  where
    !dg = U.unsafeIndex distance (ix gx gy)

    ix :: Int -> Int -> Int
    ix x y = x * m + y
    {-# INLINE ix #-}

    unIx :: Int -> (Int, Int)
    unIx = flip quotRem m

    Just (sx, sy) = unIx <$> B.findIndex (== 'S') bs
    Just (gx, gy) = unIx <$> B.findIndex (== 'G') bs

    grid :: Int -> Int -> Word8
    grid x y = B.unsafeIndex bs $ ix x y

    inGrid :: Int -> Int -> Bool
    inGrid x y = 0 <= x && x < n && 0 <= y && y < m

    isWall :: Int -> Int -> Bool
    isWall x y = grid x y == 35 -- ord '#' == 35

    !up = buildNextTable k (\i -> B.index dirs i == 'U')
    !down = buildNextTable k (\i -> B.index dirs i == 'D')
    !left = buildNextTable k (\i -> B.index dirs i == 'L')
    !right = buildNextTable k (\i -> B.index dirs i == 'R')

    neighbors :: Int -> Int -> Int -> [(Int, Int, Int)]
    neighbors t x y = do
        ((!nx, !ny), next) <- zip [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)] [up, down, left, right]
        let !nt = t + U.unsafeIndex next (rem t k) + 1
        guard $ inGrid nx ny && not (isWall nx ny) && nt < inf
        return (nt, nx, ny)


    distance :: U.Vector Int
    distance = U.create $ do
        dist <- UM.replicate (n * m) inf
        heap <- _HMnewHeap (n * m)
        UM.unsafeWrite dist (ix sx sy) 0
        _HMinsertM (0, sx, sy) heap
        fix $ \loop -> do
            top <- _HMdeleteFindMinM heap
            case top of
                Just (t, x, y) -> do
                    dxy <- UM.unsafeRead dist (ix x y)
                    if dxy == t
                    then do
                        F.for_ (neighbors t x y) $ \next@(nt, nx, ny) -> do
                            dnxny <- UM.unsafeRead dist (ix nx ny)
                            when (nt < dnxny) $ do
                                UM.unsafeWrite dist (ix nx ny) nt
                                _HMinsertM next heap
                        loop
                    else loop
                Nothing        -> return dist

inf :: Int
inf = 0x3f3f3f3f3f3f3f3f

buildNextTable :: Int -> (Int -> Bool) -> U.Vector Int
buildNextTable n p = U.take n . U.scanr' step inf $ U.generate (2 * n) id
  where
    step i acc
      | p $ rem i n = 0
      | acc < inf = acc + 1
      | otherwise = inf

data MinHeapM m a = MinHeapM (MutVar m Int) (UM.MVector m a)

_HMnewHeap :: (PrimMonad m, U.Unbox a) => Int -> m (MinHeapM (PrimState m) a)
_HMnewHeap limitSize = MinHeapM `liftM` newMutVar 0 `ap` UM.new limitSize

_HMgetHeapSize :: (PrimMonad m) => MinHeapM (PrimState m) a -> m Int
_HMgetHeapSize (MinHeapM ref _) = readMutVar ref
{-# INLINE _HMgetHeapSize #-}

_HMinsertM :: (PrimMonad m, U.Unbox a, Ord a)
    => a -> MinHeapM (PrimState m) a -> m ()
_HMinsertM x heap@(MinHeapM ref vec) = do
    size <- _HMgetHeapSize heap
    modifyMutVar' ref (+1)
    flip fix size $ \loop !i ->
        if i == 0
        then UM.unsafeWrite vec 0 x
        else do
            let !parent = (i - 1) `unsafeShiftR` 1
            p <- UM.unsafeRead vec parent
            if p <= x
            then UM.unsafeWrite vec i x
            else do
                UM.unsafeWrite vec i p
                loop parent
{-# INLINE _HMinsertM #-}

_HMunsafeDeleteMinM :: (PrimMonad m, U.Unbox a, Ord a)
                 => MinHeapM (PrimState m) a -> m ()
_HMunsafeDeleteMinM (MinHeapM ref vec) = do
    modifyMutVar' ref (subtract 1)
    size <- readMutVar ref
    x <- UM.unsafeRead vec size
    flip fix 0 $ \loop !i -> do
        let !l = unsafeShiftL i 1 .|. 1
        if size <= l
        then UM.unsafeWrite vec i x
        else do
            let !r = l + 1
            childL <- UM.unsafeRead vec l
            childR <- UM.unsafeRead vec r
            if r < size && childR < childL
            then if x <= childR
                 then UM.unsafeWrite vec i x
                 else do
                     UM.unsafeWrite vec i childR
                     loop r
            else if x <= childL
                 then UM.unsafeWrite vec i x
                 else do
                     UM.unsafeWrite vec i childL
                     loop l
{-# INLINE _HMunsafeDeleteMinM #-}

_HMunsafeMinViewM :: (PrimMonad m, U.Unbox a) => MinHeapM (PrimState m) a -> m a
_HMunsafeMinViewM (MinHeapM _ vec) = UM.unsafeRead vec 0
{-# INLINE _HMunsafeMinViewM #-}

_HMminViewM :: (PrimMonad m, U.Unbox a) => MinHeapM (PrimState m) a -> m (Maybe a)
_HMminViewM heap = do
    size <- _HMgetHeapSize heap
    if size > 0
    then Just `liftM` _HMunsafeMinViewM heap
    else return Nothing
{-# INLINE _HMminViewM #-}

_HMdeleteFindMinM :: (PrimMonad m, U.Unbox a, Ord a)
    => MinHeapM (PrimState m) a -> m (Maybe a)
_HMdeleteFindMinM heap = do
    size <- _HMgetHeapSize heap
    if size > 0
    then liftM2 ((Just.).const) (_HMunsafeMinViewM heap) (_HMunsafeDeleteMinM heap)
    else return Nothing
{-# INLINE _HMdeleteFindMinM #-}

_HMclearMinHeapM :: (PrimMonad m) => MinHeapM (PrimState m) a -> m ()
_HMclearMinHeapM (MinHeapM ref _) = writeMutVar ref 0
{-# INLINE _HMclearMinHeapM #-}
