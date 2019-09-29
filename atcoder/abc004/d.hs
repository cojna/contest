{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns, CPP, ExplicitForAll, LambdaCase, MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses, MultiWayIf, OverloadedStrings     #-}
{-# LANGUAGE Rank2Types, RecordWildCards, TupleSections, TypeFamilies #-}
{-# LANGUAGE ViewPatterns                                             #-}

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
    [r, g, b] <- map read.words <$> getLine :: IO [Int]
    print $ solve r g b

solve :: Int -> Int -> Int -> Int
solve r g b = runST $ do
    mcfb <- newMinCostFlowBuilder numV
    addEdgeMCFB sourceId rId 0 r mcfb
    addEdgeMCFB sourceId gId 0 g mcfb
    addEdgeMCFB sourceId bId 0 b mcfb
    forM_ [posMin..posMax] $ \pos -> do
        let posId = pos - posMin + 4
        addEdgeMCFB rId posId (abs $ pos - rPos) 1 mcfb
        addEdgeMCFB gId posId (abs $ pos - gPos) 1 mcfb
        addEdgeMCFB bId posId (abs $ pos - bPos) 1 mcfb
        addEdgeMCFB posId sinkId 0 1 mcfb
    mcf <- buildMinCostFlow mcfb
    Just res <- runMinCostFlow sourceId sinkId (r + g + b) mcf
    return res
  where
    numV = sinkId + 1
    sourceId = 0
    rId = 1
    gId = 2
    bId = 3
    sinkId = posMax - posMin + 4 + 1
    rPos = -100
    gPos = 0
    bPos = 100
    posMin = -600
    posMax = 600

-------------------------------------------------------------------------------
nothing :: Int
nothing = -1

inf :: Int
inf = 0x3f3f3f3f3f3f

type Vertex = Int
type Cost = Int
type Capacity = Int

data MinCostFlowBuilder s = MinCostFlowBuilder
    { numVerticesMCFB :: !Int
    , inDegreeMCFB    :: UM.MVector s Int
    , edgesMCFB       :: MutVar s [(Vertex, Vertex, Cost, Capacity)]
    }

newMinCostFlowBuilder :: (PrimMonad m)
    => Int -> m (MinCostFlowBuilder (PrimState m))
newMinCostFlowBuilder n = MinCostFlowBuilder n
    <$> UM.replicate n 0
    <*> newMutVar []

addEdgeMCFB :: (PrimMonad m)
    => Vertex -> Vertex -> Cost -> Capacity
    -> MinCostFlowBuilder (PrimState m) -> m ()
addEdgeMCFB src dst cost capacity MinCostFlowBuilder{..}
    = assert (cost >= 0) $ do
        UM.unsafeModify inDegreeMCFB (+1) src
        UM.unsafeModify inDegreeMCFB (+1) dst
        modifyMutVar' edgesMCFB ((src, dst, cost, capacity):)


buildMinCostFlow :: (PrimMonad m)
    => MinCostFlowBuilder (PrimState m) -> m (MinCostFlow (PrimState m))
buildMinCostFlow MinCostFlowBuilder{..} = do
    offsetMCF <- U.scanl' (+) 0 <$> U.unsafeFreeze inDegreeMCFB
    let numVerticesMCF = numVerticesMCFB
    let numEdgesMCF = U.last offsetMCF

    moffset <- U.thaw offsetMCF
    edges <- readMutVar edgesMCFB
    mdstMCF <- UM.replicate numEdgesMCF nothing
    mcostMCF <- UM.replicate numEdgesMCF 0
    mrevEdgeMCF <- UM.replicate numEdgesMCF nothing
    residualMCF <- UM.replicate numEdgesMCF 0

    forM_ edges $ \(src, dst, cost, capacity) -> do
        srcOffset <- UM.unsafeRead moffset src
        dstOffset <- UM.unsafeRead moffset dst
        UM.unsafeModify moffset (+1) src
        UM.unsafeModify moffset (+1) dst

        UM.unsafeWrite mdstMCF srcOffset dst
        UM.unsafeWrite mdstMCF dstOffset src
        UM.unsafeWrite mcostMCF srcOffset cost
        UM.unsafeWrite mcostMCF dstOffset (-cost)
        UM.unsafeWrite mrevEdgeMCF srcOffset dstOffset
        UM.unsafeWrite mrevEdgeMCF dstOffset srcOffset
        UM.unsafeWrite residualMCF srcOffset capacity

    dstMCF <- U.unsafeFreeze mdstMCF
    costMCF <- U.unsafeFreeze mcostMCF
    potentialMCF <- UM.replicate numVerticesMCF 0
    distMCF <- UM.replicate numVerticesMCF 0
    heapMCF <- newBinaryHeap numEdgesMCF
    revEdgeMCF <- U.unsafeFreeze mrevEdgeMCF
    prevVertexMCF <- UM.replicate numVerticesMCF nothing
    prevEdgeMCF <- UM.replicate numVerticesMCF nothing
    return MinCostFlow{..}

data MinCostFlow s = MinCostFlow
    { numVerticesMCF :: !Int
    , numEdgesMCF    :: !Int
    , offsetMCF      :: U.Vector Int
    , dstMCF         :: U.Vector Vertex
    , costMCF        :: U.Vector Cost
    , residualMCF    :: UM.MVector s Capacity
    , potentialMCF   :: UM.MVector s Cost
    , distMCF        :: UM.MVector s Cost
    , heapMCF        :: BinaryHeap s Word64 -- (Cost, Vertex)
    , revEdgeMCF     :: U.Vector Int
    , prevVertexMCF  :: UM.MVector s Vertex
    , prevEdgeMCF    :: UM.MVector s Int
    }
-- | Primal Dual O(FElog V)
runMinCostFlow :: (PrimMonad m)
    => Vertex -> Vertex -> Capacity -> MinCostFlow (PrimState m) -> m (Maybe Cost)
runMinCostFlow source sink flow mcf@MinCostFlow{..} = go 0 flow
  where
    go !res !f
        | f == 0 = return $ Just res
        | otherwise = do
            canFlow <- dijkstraMCF source sink mcf
            if canFlow
            then do
                rep numVerticesMCF $ \v -> do
                    dv <- UM.unsafeRead distMCF v
                    UM.unsafeModify potentialMCF (+dv) v
                flowed <- updateResidualMCF sink f mcf
                hsink <- UM.unsafeRead potentialMCF sink
                go (hsink * flowed + res) (f - flowed)
            else return Nothing

encodeMCF :: Cost -> Vertex -> Word64
encodeMCF cost v = unsafeCoerce $ unsafeShiftL cost 16 .|. v
{-# INLINE encodeMCF #-}

decodeMCF :: Word64 -> (Cost, Vertex)
decodeMCF costv = unsafeCoerce (cost, v)
  where
    !cost = unsafeShiftR costv 16
    !v = costv .&. 0xffff
{-# INLINE decodeMCF #-}

dijkstraMCF :: (PrimMonad m)
    => Vertex -> Vertex -> MinCostFlow (PrimState m) -> m Bool
dijkstraMCF source sink MinCostFlow{..} = do
    UM.set distMCF inf
    UM.unsafeWrite distMCF source 0
    clearBH heapMCF
    insertMinBH (encodeMCF 0 source) heapMCF

    fix $ \loop -> do
        deleteFindMinBH heapMCF >>= \case
            Just cv -> do
                let (c, v) = decodeMCF cv
                dv <- UM.unsafeRead distMCF v
                unless (c > dv) $ do
                    let start = U.unsafeIndex offsetMCF v
                    let end   = U.unsafeIndex offsetMCF (v + 1)
                    U.forM_ (U.generate (end - start) (+start)) $ \e -> do
                        let nv = U.unsafeIndex dstMCF e
                        let v2nv = U.unsafeIndex costMCF e
                        cap  <- UM.unsafeRead residualMCF e
                        hv   <- UM.unsafeRead potentialMCF v
                        hnv  <- UM.unsafeRead potentialMCF nv
                        old  <- UM.unsafeRead distMCF nv
                        let dnv = dv + v2nv + hv - hnv
                        when (cap > 0 && dnv < old) $ do
                            UM.unsafeWrite distMCF nv dnv
                            UM.unsafeWrite prevVertexMCF nv v
                            UM.unsafeWrite prevEdgeMCF nv e
                            insertMinBH (encodeMCF dnv nv) heapMCF
                loop
            Nothing -> do
                cost <- UM.unsafeRead distMCF sink
                return $! cost < inf
{-# INLINE dijkstraMCF #-}

updateResidualMCF :: (PrimMonad m)
    => Vertex -> Capacity -> MinCostFlow (PrimState m) -> m Capacity
updateResidualMCF sink flow MinCostFlow{..} = go sink flow return
  where
    go !v !f k = do
        pv <- UM.unsafeRead prevVertexMCF v
        if pv < 0
        then k f
        else do
            pv2v <- UM.unsafeRead prevEdgeMCF v
            f' <- UM.unsafeRead residualMCF pv2v
            go pv (min f f') $ \nf -> do
                UM.unsafeModify residualMCF (subtract nf) pv2v
                UM.unsafeModify residualMCF (+nf) (U.unsafeIndex revEdgeMCF pv2v)
                k nf
{-# INLINE updateResidualMCF #-}

data BinaryHeap s a = BinaryHeap (MutVar s Int) (UM.MVector s a)

newBinaryHeap :: (PrimMonad m, U.Unbox a) => Int -> m (BinaryHeap (PrimState m) a)
newBinaryHeap n = BinaryHeap <$> newMutVar 0 <*> UM.new n

getBinaryHeapSize :: (PrimMonad m) => BinaryHeap (PrimState m) a -> m Int
getBinaryHeapSize (BinaryHeap ref _) = readMutVar ref
{-# INLINE getBinaryHeapSize #-}

siftUp :: (PrimMonad m, U.Unbox a, Ord a) => Int -> UM.MVector (PrimState m) a -> m ()
siftUp k vec = do
    x <- UM.unsafeRead vec k
    flip fix k $ \loop !i ->
        if i > 0
        then do
            let !parent = (i - 1) `unsafeShiftR` 1
            p <- UM.unsafeRead vec parent
            if p <= x
            then UM.unsafeWrite vec i x
            else do
                UM.unsafeWrite vec i p
                loop parent
        else UM.unsafeWrite vec 0 x
{-# INLINE siftUp #-}

siftDown :: (PrimMonad m, U.Unbox a, Ord a) => Int -> UM.MVector (PrimState m) a -> m ()
siftDown k vec = do
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
            if r < n && childR < childL
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
{-# INLINE siftDown #-}

heapify :: (PrimMonad m, U.Unbox a, Ord a) => UM.MVector (PrimState m) a -> m ()
heapify vec = do
    rev (UM.length vec `quot` 2) $ \i -> do
        siftDown i vec
{-# INLINE heapify #-}

buildBinaryHeap :: (PrimMonad m, U.Unbox a, Ord a)
    => U.Vector a -> m (BinaryHeap (PrimState m) a)
buildBinaryHeap vec = do
    ref <- newMutVar $ U.length vec
    mvec <- U.unsafeThaw vec
    heapify mvec
    return $! BinaryHeap ref mvec
{-# INLINE buildBinaryHeap #-}

unsafeMinViewBH :: (PrimMonad m, U.Unbox a) => BinaryHeap (PrimState m) a -> m a
unsafeMinViewBH (BinaryHeap _ vec) = UM.unsafeRead vec 0
{-# INLINE unsafeMinViewBH #-}

minViewBH :: (PrimMonad m, U.Unbox a)
    => BinaryHeap (PrimState m) a -> m (Maybe a)
minViewBH bh = do
    size <- getBinaryHeapSize bh
    if size > 0
    then Just <$!> unsafeMinViewBH bh
    else return $! Nothing
{-# INLINE minViewBH #-}

insertMinBH :: (PrimMonad m, U.Unbox a, Ord a)
    => a -> BinaryHeap (PrimState m) a -> m ()
insertMinBH x bh@(BinaryHeap info vec) = do
    size <- getBinaryHeapSize bh
    modifyMutVar' info (+1)
    UM.unsafeWrite vec size x
    siftUp size vec
{-# INLINE insertMinBH #-}

unsafeDeleteMinBH :: (PrimMonad m, U.Unbox a, Ord a)
    => BinaryHeap (PrimState m) a -> m ()
unsafeDeleteMinBH bh@(BinaryHeap info vec) = do
    size <- getBinaryHeapSize bh
    modifyMutVar' info (subtract 1)
    UM.unsafeSwap vec 0 (size - 1)
    siftDown 0 (UM.unsafeTake (size - 1) vec)
{-# INLINE unsafeDeleteMinBH #-}

modifyMinBH :: (PrimMonad m, U.Unbox a, Ord a)
    => BinaryHeap (PrimState m) a -> (a -> a) -> m ()
modifyMinBH bh@(BinaryHeap _ vec) f = do
    UM.unsafeModify vec f 0
    size <- getBinaryHeapSize bh
    siftDown 0 (UM.unsafeTake size vec)
{-# INLINE modifyMinBH #-}

deleteFindMinBH :: (PrimMonad m, U.Unbox a, Ord a)
    => BinaryHeap (PrimState m) a -> m (Maybe a)
deleteFindMinBH bh@(BinaryHeap _ vec) = do
    size <- getBinaryHeapSize bh
    if size > 0
    then Just <$!> unsafeMinViewBH bh <* unsafeDeleteMinBH bh
    else return $! Nothing
{-# INLINE deleteFindMinBH #-}

clearBH :: (PrimMonad m) => BinaryHeap (PrimState m) a -> m ()
clearBH (BinaryHeap info _) = writeMutVar info 0

freezeInternalBinaryHeapBH :: (PrimMonad m, U.Unbox a)
    => BinaryHeap (PrimState m) a -> m (U.Vector a)
freezeInternalBinaryHeapBH bh@(BinaryHeap _ vec) = do
    size <- getBinaryHeapSize bh
    U.unsafeFreeze (UM.unsafeTake size vec)

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
