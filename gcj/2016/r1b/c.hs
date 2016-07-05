{-# OPTIONS_GHC -O2 -funbox-strict-fields #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Control.Monad.State.Strict
import           Control.Parallel.Strategies
import           Data.Bool
import qualified Data.ByteString.Char8       as B
import qualified Data.ByteString.Unsafe      as B
import           Data.Char
import           Data.Fixed
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
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import qualified Data.Vector                 as V
import qualified Data.Vector.Mutable         as VM
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM
import           Debug.Trace
import           Foreign                     hiding (void)
import           System.IO
import           Unsafe.Coerce

solve :: Task -> Answer
solve xys = length xys - (flow + (numX - flow) + (numY - flow))
  where
    (xs, ys) = unzip $ convert xys
    numX = maximum xs + 1
    numY = maximum ys + 1
    source = numX + numY
    sink = source + 1
    numV = numX + numY + 2
    edges = map (\x -> (source, x, 1)) [0..numX-1]
      ++ map (\(x, y) -> (x, y, 1)) (zip xs $ map (+numX) ys)
      ++ map (\y -> (y, sink, 1)) [numX..numX+numY-1]
    flow = maxFlow numV (U.fromList edges) source sink


convert :: [(String, String)] -> [(Int, Int)]
convert xys = zip (toIDs xs) $ toIDs ys
  where
    (xs, ys) = unzip xys

toIDs :: (Ord a) => [a] -> [Int]
toIDs = go 0 M.empty
  where
    go !i !used (x:xs)
      | Just j<-M.lookup x used = j : go i used xs
      | otherwise = i : go (i+1) (M.insert x i used) xs
    go _ _ _ = []

type Task = [(String,String)]
getTask :: IO Task
getTask = do
    n <- readLn
    replicateM n $ do
      [xs, ys] <- words <$> getLine
      return (xs, ys)

type Answer = Int
putAnswer :: Answer -> IO ()
putAnswer = print

main :: IO ()
main = do
  t <- readLn
  start <- getPOSIXTime
  answers <- parMap rdeepseq solve <$> replicateM t getTask
  foldM_ `flip` start `flip` (zip [1..] answers)$ \prev (i, answer) -> do
      putStr $ "Case #" ++ shows i ": "
      putAnswer answer
      cur <- getPOSIXTime
      hPutStr stderr $ shows i "/" ++ shows t ": "
      hPutStrLn stderr $ (shows.msec) (cur - prev) "ms"
      return cur

msec :: NominalDiffTime -> Int
msec s = let t = realToFrac s :: Milli in fromEnum t

data QueueM m a = QM (MutVar m [a]) (MutVar m [a])

newQueueM:: (PrimMonad m) => m (QueueM (PrimState m) a)
newQueueM = QM `liftM` newMutVar [] `ap` newMutVar []

dequeueM :: (PrimMonad m) => QueueM (PrimState m) a -> m (Maybe a)
dequeueM (QM front rear) = do
  fs <- readMutVar front
  case fs of
    (f:fs') -> writeMutVar front fs' >> return (Just f)
    [] -> do
      rs <- readMutVar rear
      writeMutVar rear []
      case reverse rs of
        (r:rs') -> writeMutVar front rs' >> return (Just r)
        [] -> return Nothing
{-# INLINE dequeueM #-}

enqueueM :: (PrimMonad m) => a -> QueueM (PrimState m) a -> m ()
enqueueM x (QM _ rear) = modifyMutVar' rear (x:)
{-# INLINE enqueueM #-}

type Vertex = Int
type EdgeIndex = Int
type Capacity = Int
type Edge = (Vertex, Vertex, Capacity)
type Graph = V.Vector (U.Vector (Vertex, EdgeIndex))

inf :: Int
inf = 0x3f3f3f3f

nothing :: Int
nothing = -1

maxFlow :: Int -> U.Vector Edge -> Vertex -> Vertex -> Capacity
maxFlow numV edges source sink = runST $ do
    (gr, res) <- buildResidualGraph numV edges
    level <- UM.replicate (V.length gr) nothing
    iter <- UM.replicate (V.length gr) 0

    flip fix 0 $ \dinicLoop flow -> do
        maxFlowBFS gr res level source
        l <- UM.unsafeRead level sink
        if l == nothing then return flow
        else do
            UM.set iter 0
            flip fix flow $ \dfsLoop !acc -> do
                f <- maxFlowDFS gr res level iter source sink
                if f > 0
                then dfsLoop $ acc + f
                else dinicLoop acc

reverseEdgeIndex :: Int -> Int
reverseEdgeIndex = xor 1
{-# INLINE reverseEdgeIndex #-}

buildResidualGraph :: Int
                   -> U.Vector Edge
                   -> ST s (Graph, UM.MVector s Capacity)
buildResidualGraph numV edges = do
    gr <- VM.replicate numV []
    residual <- UM.replicate (2 * U.length edges) 0
    U.foldM'_ `flip` 0 `flip` edges $ \i (src, dst, cap) -> do
        VM.unsafeModify gr ((dst,i):) src >> VM.unsafeModify gr ((src,i+1):) dst
        UM.unsafeWrite residual i cap
        return $ i + 2
    flip (,) residual . V.map U.fromList <$> V.unsafeFreeze gr

maxFlowBFS :: Graph
           -> UM.MVector s Int
           -> UM.MVector s Int
           -> Vertex
           -> ST s ()
maxFlowBFS gr res level source = do
    UM.set level nothing
    queue <- newQueueM
    UM.unsafeWrite level source 0
    enqueueM source queue
    fix $ \bfs -> do
        hd <- dequeueM queue
        case hd of
            Just v -> do
                U.forM_ (V.unsafeIndex gr v) $ \(u, i) -> do
                    lu <- UM.unsafeRead level u
                    cap <- UM.unsafeRead res i
                    when (cap > 0 && lu == nothing) $ do
                        lv <- UM.unsafeRead level v
                        UM.unsafeWrite level u $ lv + 1
                        enqueueM u queue
                bfs
            Nothing -> return ()

maxFlowDFS :: Graph
           -> UM.MVector s Int
           -> UM.MVector s Int
           -> UM.MVector s Int
           -> Vertex
           -> Vertex
           -> ST s Capacity
maxFlowDFS gr res level iter source sink =
    fix `flip` source `flip` sink `flip` inf $ \dfs !v !t !f ->
        if v == t then return f
        else do
            i0 <- UM.unsafeRead iter v
            flip fix (U.unsafeDrop i0 $ V.unsafeIndex gr v)$ \loop vec ->
                if U.length vec > 0
                then do
                    let (u, j) = U.unsafeHead vec
                    UM.unsafeModify iter (+1) v
                    cap <- UM.unsafeRead res j
                    lv <- UM.unsafeRead level v
                    lu <- UM.unsafeRead level u
                    if cap > 0 && lv < lu
                    then do
                        d <- dfs u t (min f cap)
                        if d > 0
                        then do
                            UM.unsafeModify res (subtract d) j
                            let k = reverseEdgeIndex j
                            UM.unsafeModify res (+d) k
                            return d
                        else loop $ U.unsafeTail vec
                    else loop $ U.unsafeTail vec
                else return 0