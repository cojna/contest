{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns, CPP, LambdaCase, MagicHash                   #-}
{-# LANGUAGE MultiParamTypeClasses, MultiWayIf, OverloadedStrings       #-}
{-# LANGUAGE RecordWildCards, TupleSections, TypeFamilies, ViewPatterns #-}

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
import           Data.Monoid                 hiding (First)
import           Data.Ord
import           Data.Primitive.MutVar
import           Data.Ratio
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
    es <- U.unfoldrN (n - 1) parseInt2 <$> C.getContents
    print $ solve n es

data Answer = First | Second deriving Show

solve :: Int -> U.Vector (Int, Int) -> Answer
solve n es
    | diameter gr `mod` 3 == 1 = Second
    | otherwise = First
  where
    gr = buildUndirectedGraphW n
        $ U.map (\(x, y) -> (x - 1, y - 1, (1 :: Int))) es
--
type Vertex = Int
type Edge = (Vertex, Vertex)
type EdgeWith w = (Vertex, Vertex, w)
data SparseGraph w = CSR
    { numVerticesCSR :: !Int
    , numEdgesCSR    :: !Int
    , offsetCSR      :: !(U.Vector Int)
    , adjacentCSR    :: !(U.Vector Vertex)
    , edgeCtxCSR     :: !(U.Vector w)
    }

buildDirectedGraph
    :: Int -> U.Vector Edge -> SparseGraph ()
buildDirectedGraph numVerticesCSR edges = runST $ do
    let numEdgesCSR = U.length edges
    let offsetCSR = U.scanl' (+) 0
            . U.unsafeAccumulate (+) (U.replicate numVerticesCSR 0)
            . U.map (flip (,) 1)
            . fst
            $ U.unzip edges
    moffset <- U.thaw offsetCSR
    madj <- UM.new numEdgesCSR
    U.forM_ edges $ \(src, dst) -> do
        pos <- UM.unsafeRead moffset src
        UM.unsafeWrite moffset src (pos + 1)
        UM.unsafeWrite madj pos dst
    adjacentCSR <- U.unsafeFreeze madj
    return CSR{edgeCtxCSR = U.replicate numEdgesCSR (), ..}

buildUndirectedGraph :: Int -> U.Vector Edge -> SparseGraph ()
buildUndirectedGraph n edges
    = buildDirectedGraph n (edges U.++ U.map swap edges)

buildDirectedGraphW :: (U.Unbox w)
    => Int -> U.Vector (EdgeWith w) -> SparseGraph w
buildDirectedGraphW numVerticesCSR edges = runST $ do
    let numEdgesCSR = U.length edges
    let offsetCSR = U.scanl' (+) 0
            . U.unsafeAccumulate (+) (U.replicate numVerticesCSR 0)
            . U.map (flip (,) 1)
            . (\(x, _, _) -> x)
            $ U.unzip3 edges
    moffset <- U.thaw offsetCSR
    madj <- UM.new numEdgesCSR
    mectx <- UM.new numEdgesCSR
    U.forM_ edges $ \(src, dst, w) -> do
        pos <- UM.unsafeRead moffset src
        UM.unsafeWrite moffset src (pos + 1)
        UM.unsafeWrite madj pos dst
        UM.unsafeWrite mectx pos w
    adjacentCSR <- U.unsafeFreeze madj
    edgeCtxCSR <- U.unsafeFreeze mectx
    return CSR{..}

buildUndirectedGraphW :: (U.Unbox w)
    => Int -> U.Vector (EdgeWith w) -> SparseGraph w
buildUndirectedGraphW numVerticesCSR edges = runST $ do
    let numEdgesCSR = 2 * U.length edges
    outDeg <- UM.replicate numVerticesCSR (0 :: Int)
    U.forM_ edges $ \(x, y, _) -> do
        UM.unsafeModify outDeg (+1) x
        UM.unsafeModify outDeg (+1) y
    offsetCSR <- U.scanl' (+) 0 <$> U.unsafeFreeze outDeg
    moffset <- U.thaw offsetCSR
    madj <- UM.new numEdgesCSR
    mectx <- UM.new numEdgesCSR
    U.forM_ edges $ \(x, y, w) -> do
        posX <- UM.unsafeRead moffset x
        posY <- UM.unsafeRead moffset y
        UM.unsafeWrite moffset x (posX + 1)
        UM.unsafeWrite moffset y (posY + 1)
        UM.unsafeWrite madj posX y
        UM.unsafeWrite madj posY x
        UM.unsafeWrite mectx posX w
        UM.unsafeWrite mectx posY w
    adjacentCSR <- U.unsafeFreeze madj
    edgeCtxCSR <- U.unsafeFreeze mectx
    return CSR{..}

adj :: SparseGraph w -> Vertex -> U.Vector Vertex
adj CSR{..} v = U.unsafeSlice o (o' - o) adjacentCSR
  where
    o = U.unsafeIndex offsetCSR v
    o' = U.unsafeIndex offsetCSR (v + 1)
{-# INLINE adj #-}

iadj :: SparseGraph w -> Vertex -> U.Vector (Int, Vertex)
iadj CSR{..} v = U.imap ((,) . (+o)) $ U.unsafeSlice o (o' - o) adjacentCSR
  where
    o = U.unsafeIndex offsetCSR v
    o' = U.unsafeIndex offsetCSR (v + 1)
{-# INLINE iadj #-}

adjW :: (U.Unbox w)
    => SparseGraph w -> Vertex -> U.Vector (Vertex, w)
adjW CSR{..} v = U.zip
    (U.unsafeSlice o (o' - o) adjacentCSR)
    (U.unsafeSlice o (o' - o) edgeCtxCSR)
  where
    o = U.unsafeIndex offsetCSR v
    o' = U.unsafeIndex offsetCSR (v + 1)
{-# INLINE adjW #-}

iadjW :: (U.Unbox w)
    => SparseGraph w -> Vertex -> U.Vector (Int, Vertex, w)
iadjW CSR{..} v = U.izipWith (\i u w -> (i + o, u, w))
    (U.unsafeSlice o (o' - o) adjacentCSR)
    (U.unsafeSlice o (o' - o) edgeCtxCSR)
  where
    o = U.unsafeIndex offsetCSR v
    o' = U.unsafeIndex offsetCSR (v + 1)
{-# INLINE iadjW #-}

outEdges :: SparseGraph w -> Vertex -> U.Vector Int
outEdges CSR{..} v = U.generate (o' - o) (+o)
  where
    o = U.unsafeIndex offsetCSR v
    o' = U.unsafeIndex offsetCSR (v + 1)
{-# INLINE outEdges #-}


shortestPath :: (U.Unbox w, Num w) => SparseGraph w -> Vertex -> U.Vector w
shortestPath gr root = U.create $ do
    let n = numVerticesCSR gr
    dist <- UM.unsafeNew n
    UM.unsafeWrite dist root 0
    stack <- newVecStack n
    parent <- UM.unsafeNew n

    U.forM_ (gr `iadjW` root) $ \(ei, v, d) -> do
        push ei stack
        UM.unsafeWrite parent v root
        UM.unsafeWrite dist v d

    fix $ \loop ->
        pop stack >>= \case
            Just ei -> do
                let v = adjacentCSR gr `U.unsafeIndex` ei
                pv <- UM.unsafeRead parent v
                dv <- UM.unsafeRead dist v
                U.forM_ (gr `iadjW` v) $ \(nei, nv, d) -> do
                    when (pv /= nv) $ do
                        push nei stack
                        UM.unsafeWrite parent nv v
                        UM.unsafeWrite dist nv $ dv + d
                loop
            Nothing -> return ()
    return dist

diameter :: (U.Unbox w, Ord w, Num w) => SparseGraph w -> w
diameter tree = U.maximum
    . shortestPath tree
    . U.maxIndex
    $ shortestPath tree 0


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
-- Data.VecStack
-------------------------------------------------------------------------------
data VecStack s a = VecStack{stackInfo :: !(UM.MVector s Int), stackData :: !(UM.MVector s a)}
newVecStack :: (PrimMonad m, UM.Unbox a) => Int -> m (VecStack (PrimState m) a)
newVecStack n = VecStack <$> UM.replicate 1 0 <*> UM.unsafeNew n
defaultVecStackSize :: Int
defaultVecStackSize = 1024 * 1024
pop :: (PrimMonad m, UM.Unbox a) => VecStack (PrimState m) a -> m (Maybe a)
pop (VecStack info s) = do { len <- UM.unsafeRead info 0; if len > 0 then do { UM.unsafeWrite info 0 (len - 1); pure <$> UM.unsafeRead s (len - 1)} else return Nothing}
{-# INLINE pop #-}
push :: (PrimMonad m, UM.Unbox a) => a -> VecStack (PrimState m) a -> m ()
push x (VecStack info s) = do { len <- UM.unsafeRead info 0; UM.unsafeWrite s len x; UM.unsafeWrite info 0 (len + 1)}
{-# INLINE push #-}
