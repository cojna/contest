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
import qualified Data.Primitive.ByteArray    as BA
import qualified Data.Set                    as S
import           Data.Tuple
import qualified Data.Vector                 as V
import qualified Data.Vector.Mutable         as VM
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Primitive       as PV
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Base    as UB
import qualified Data.Vector.Unboxed.Mutable as UM
import           Debug.Trace
import           Foreign                     hiding (void)
import           GHC.Exts
import qualified System.IO                   as IO
import           Unsafe.Coerce

main :: IO ()
main = do
    !n <- readLn :: IO Int
    mat <- U.unfoldrN (n*(n-1)) parseInt <$> C.getContents
    print . maybe (-1) id $ solve n $ V.generate n $ \i -> U.slice (i * (n - 1)) (n - 1) mat

type TaskId = Int

taskId :: Int -> Int -> TaskId
taskId x y
    | x < y = x * 1024 + y
    | otherwise = y * 1024 + x

solve :: Int -> V.Vector (U.Vector Int) -> Maybe Int
solve n (V.imap (U.map . taskId . (+1)) -> priority0) = runST $ do
    let numV = 1024 * 1024
    let !gr = buildDirectedGraph numV
            . U.convert
            $ V.concatMap (\row -> U.convert . U.zip row $ U.tail row ) priority0
    let !inDegree = U.unsafeAccumulate (+) (U.replicate numV (0 :: Int))
            . U.map (flip (,) 1)
            $ U.drop (numV + 1) gr

    queue <- newIntQueue
    let tasks0 = IS.toList
            . V.foldl' (flip IS.insert) IS.empty
            . V.filter ((== 0) . U.unsafeIndex inDegree)
            $ V.map U.unsafeHead priority0

    forM_ tasks0 $ \task -> do
        enqueueIQ task queue
    inDeg <- U.unsafeThaw inDegree

    flip fix 0 $ \loop !i -> do
        nullIQ queue >>= \case
            True -> do
                count <- getEnqueueCount queue
                if count == n * (n - 1) `div` 2
                then return $ Just i
                else return Nothing
            False -> do
                tasks <- dequeueAllIQ queue
                U.forM_ tasks $ \task -> do
                    U.forM_ (adjacent gr task) $ \child -> do
                        d <- UM.unsafeRead inDeg child
                        UM.unsafeWrite inDeg child (d - 1)
                        when (d == 1) $ do
                            enqueueIQ child queue
                loop (i + 1)

newtype IntQueue s = IntQueue {runIntQueue :: BA.MutableByteArray s}

newIntQueue :: (PrimMonad m) => m (IntQueue (PrimState m))
newIntQueue = IntQueue <$> BA.newByteArray (2 * 1024 * 1024 * 8)

getEnqueueCount :: (PrimMonad m) => IntQueue (PrimState m) -> m Int
getEnqueueCount queue = BA.readByteArray (runIntQueue queue) 1
{-# INLINE getEnqueueCount #-}

getDequeueCount :: (PrimMonad m) => IntQueue (PrimState m) -> m Int
getDequeueCount queue = BA.readByteArray (runIntQueue queue) 0
{-# INLINE getDequeueCount #-}

lengthIQ :: (PrimMonad m) => IntQueue (PrimState m) -> m Int
lengthIQ queue = (-) <$> getEnqueueCount queue <*> getDequeueCount queue

nullIQ :: (PrimMonad m) => IntQueue (PrimState m) -> m Bool
nullIQ queue = (== 0) <$> lengthIQ queue

enqueueIQ :: (PrimMonad m) => Int -> IntQueue (PrimState m) -> m ()
enqueueIQ x queue = do
    r <- getEnqueueCount queue
    BA.writeByteArray (runIntQueue queue) 1 (r + 1)
    BA.writeByteArray (runIntQueue queue) (r + 2) x

dequeueIQ :: (PrimMonad m) => IntQueue (PrimState m) -> m (Maybe Int)
dequeueIQ queue = do
    f <- getDequeueCount queue
    r <- getEnqueueCount queue
    if f < r
    then do
        BA.writeByteArray (runIntQueue queue) 0 (f + 1)
        Just <$!> BA.readByteArray (runIntQueue queue) (f + 2)
    else return Nothing

dequeueAllIQ :: (PrimMonad m) => IntQueue (PrimState m) -> m (U.Vector Int)
dequeueAllIQ queue = do
    f <- getDequeueCount queue
    r <- getEnqueueCount queue
    BA.writeByteArray (runIntQueue queue) 0 r
    buf <- BA.newByteArray (8 * (r - f))
    BA.copyMutableByteArray buf 0 (runIntQueue queue) ((f + 2) * 8) ((r - f) * 8)
    unsafeCoerce . PV.Vector 0 (r - f) <$> BA.unsafeFreezeByteArray buf

unsafeFreezeIQ :: (PrimMonad m) => IntQueue (PrimState m) -> m (U.Vector Int)
unsafeFreezeIQ queue = do
    f <- getDequeueCount queue
    r <- getEnqueueCount queue
    unsafeCoerce . PV.Vector f (r - f) <$> BA.unsafeFreezeByteArray (runIntQueue queue)

-------------------------------------------------------------------------------
type Vertex = Int
type Edge = (Vertex, Vertex)
type Graph = U.Vector Int

buildDirectedGraph :: Int -> U.Vector Edge -> Graph
buildDirectedGraph n edges = U.create $ do
    offset <- U.unsafeThaw
        . U.scanl' (+) (n + 1)
        . U.unsafeAccumulate (+) (U.replicate n 0)
        . U.map (flip (,) 1)
        . fst
        $ U.unzip edges
    gr <- UM.replicate (UM.length offset + U.length edges) 0
    U.forM_ (U.generate (n + 1) id) $ \i ->
        UM.unsafeRead offset i >>= UM.unsafeWrite gr i
    U.forM_ edges $ \(src, dst) -> do
        pos <- UM.unsafeRead offset src
        UM.unsafeWrite offset src (pos + 1)
        UM.unsafeWrite gr pos dst
    return gr

buildUndirectedGraph :: Int -> U.Vector Edge -> Graph
buildUndirectedGraph n edges
    = buildDirectedGraph n (edges U.++ U.map swap edges)

numVertices :: Graph -> Int
numVertices gr = U.head gr - 1

numEdges :: Graph -> Int
numEdges gr = U.length gr - U.head gr

vertices :: Graph -> U.Vector Vertex
vertices = flip U.generate id . numVertices

adjacent :: Graph -> Int -> U.Vector Vertex
adjacent gr v = U.unsafeSlice offset len gr
  where
    offset = U.unsafeIndex gr v
    len = U.unsafeIndex gr (v + 1) - offset

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