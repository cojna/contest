{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
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
import qualified Data.ByteString.Char8       as C
import qualified Data.ByteString.Unsafe      as B
import           Data.Char
import qualified Data.Foldable               as F
import           Data.Function
import qualified Data.IntMap.Strict          as IM
import qualified Data.IntSet                 as IS
import qualified Data.List                   as L
import qualified Data.Map.Strict             as M
import           Data.Monoid                 hiding (First(..))
import           Data.Ord
import           Data.Primitive.MutVar
import qualified Data.Set                    as S
import           Data.Tuple
import qualified Data.Vector                 as V
import qualified Data.Vector.Mutable         as VM
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM
import           Debug.Trace
import           Foreign                     hiding (void)
import           GHC.Exts
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
    gr = undirectedWGraph (U.replicate n ())
        $ U.map (\(x, y) -> (x - 1, y - 1, 1)) es

type VertexId = Int
type EdgeId = Int
type Vertex = ()
type Edge = (Int, Int)
type WEdge = (Int, Int, Int)

data Graph v e = Graph
    { offset :: !(U.Vector Int)
    , vertices :: !(U.Vector v)
    , edges :: !(U.Vector e)
    }

adj :: (U.Unbox v, U.Unbox e) => Graph v e -> VertexId -> U.Vector e
adj Graph{..} v = U.unsafeSlice o (o' - o) edges
  where
    o  = U.unsafeIndex offset v
    o' = U.unsafeIndex offset (v + 1)
{-# INLINE adj #-}

iadj :: (U.Unbox v, U.Unbox e) => Graph v e -> VertexId -> U.Vector (Int, e)
iadj Graph{..} v = U.imap (\i e -> (o + i, e))
    $ U.unsafeSlice o (o' - o) edges
  where
    o  = U.unsafeIndex offset v
    o' = U.unsafeIndex offset (v + 1)
{-# INLINE iadj #-}

undirectedWGraph
    :: (U.Unbox v)
    => U.Vector v -> U.Vector (Int, Int, Int) -> Graph v WEdge
undirectedWGraph vertices es = runST $ do
    mbuf <- UM.unsafeNew numE
    outDeg <- UM.replicate numV 0
    flip U.imapM_ es $ \i (x, y, d) -> do
        UM.unsafeModify outDeg (+1) x
        UM.unsafeModify outDeg (+1) y
        UM.unsafeWrite mbuf (2 * i) (x, y, d)
        UM.unsafeWrite mbuf (2 * i + 1) (y, x, d)
    offset <- U.scanl' (+) 0 <$> U.unsafeFreeze outDeg
    buf <- U.unsafeFreeze mbuf

    mpos <- U.thaw offset
    medges <- UM.unsafeNew numE
    U.forM_ buf $ \e@(src, _, _) -> do
        p <- UM.unsafeRead mpos src
        UM.unsafeWrite medges p e
        UM.unsafeModify mpos (+1) src
    edges <- U.unsafeFreeze medges
    _ <- U.unsafeFreeze mpos
    return $! Graph{..}
  where
    numV = U.length vertices
    numE = 2 * U.length es

shortestPath :: (U.Unbox v) => Graph v WEdge -> VertexId -> U.Vector Int
shortestPath gr@Graph{..} root = U.create $ do
    dist <- UM.unsafeNew (U.length vertices)
    UM.unsafeWrite dist root 0
    stack <- newStackM
    U.forM_ (iadj gr root) $ \(i, _) ->
        pushM i stack

    fix $ \loop ->
        popM stack >>= \case
            Just ei -> do
                let (p, v, d) = U.unsafeIndex edges ei
                UM.unsafeRead dist p >>= UM.unsafeWrite dist v . (+d)
                let nexts = U.map fst
                        . U.filter (\(_, (_, dst, _)) -> dst /= p)
                        $ iadj gr v
                U.forM_ nexts $ \i ->
                    pushM i stack
                loop
            Nothing -> return ()
    return dist

diameter :: (U.Unbox v) => Graph v WEdge -> Int
diameter tree = U.maximum
    . shortestPath tree
    . U.maxIndex
    $ shortestPath tree 0

height :: (U.Unbox v) => Graph v WEdge -> U.Vector Int
height tree = U.zipWith max fromS fromT
  where
    s = U.maxIndex $ shortestPath tree 0
    fromS = shortestPath tree s
    t = U.maxIndex fromS
    fromT = shortestPath tree t

data VecStack m a = VecStack
    { stackInfo :: !(UM.MVector m Int)
    , stackData :: !(UM.MVector m a)
    }

newStackM :: (PrimMonad m, U.Unbox a) => m (VecStack (PrimState m) a)
newStackM = VecStack <$> UM.replicate 1 0 <*> UM.unsafeNew (1024 * 1024)

popM :: (PrimMonad m, U.Unbox a) => VecStack (PrimState m) a -> m (Maybe a)
popM (VecStack info s) = do
    len <- UM.unsafeRead info 0
    if len > 0
    then do
        UM.unsafeWrite info 0 (len - 1)
        pure <$> UM.unsafeRead s (len - 1)
    else return Nothing
{-# INLINE popM #-}

pushM :: (PrimMonad m, U.Unbox a) => a -> VecStack (PrimState m) a -> m ()
pushM x (VecStack info s) = do
    len <- UM.unsafeRead info 0
    UM.unsafeWrite s len x
    UM.unsafeWrite info 0 (len + 1)
{-# INLINE pushM #-}

-------------------------------------------------------------------------------
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