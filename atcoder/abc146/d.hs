{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns, CPP, FlexibleContexts, FlexibleInstances         #-}
{-# LANGUAGE KindSignatures, LambdaCase, MagicHash, MultiParamTypeClasses   #-}
{-# LANGUAGE MultiWayIf, OverloadedStrings, RecordWildCards                 #-}
{-# LANGUAGE ScopedTypeVariables, TupleSections, TypeFamilies, ViewPatterns #-}

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
import           Data.Functor.Identity
import qualified Data.IntMap.Strict          as IM
import qualified Data.IntSet                 as IS
import qualified Data.List                   as L
import qualified Data.Map.Strict             as M
import           Data.Monoid
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
    es <- U.unfoldrN (n - 1) (runStateT $ (,) <$> int1 <*> int1) <$> C.getContents
    let (k, cs) = solve n es
    print k
    putStr.unlines.map show $ U.toList cs

solve :: Int -> U.Vector (Int, Int) -> (Int, U.Vector Int)
solve n es |traceShow (ps, cs) True= (k, U.map color es)
  where
    !gr = buildUndirectedGraph n es
    !k = U.maximum $ U.generate n (U.length . outEdges gr)
    (!ps, !cs) = runST $ do
        colors <- UM.replicate n (-1)
        parent <- UM.replicate n (-1)
        let root = 0
        flip fix root $ \dfs v -> do
            p <- UM.unsafeRead parent v
            let !deg = U.length (gr `adj` v)
            U.forM_ (U.indexed . U.filter (/=p) $ gr `adj` v) $ \(c, nv) -> do
                UM.unsafeWrite parent nv v
                cv <- UM.unsafeRead colors v
                if cv /= c + 1
                then UM.unsafeWrite colors nv (c + 1)
                else UM.unsafeWrite colors nv deg
                dfs nv
        (,) <$> U.unsafeFreeze parent
            <*> U.unsafeFreeze colors
    color (pv, v)
      | U.unsafeIndex ps v == pv = U.unsafeIndex cs v
      | otherwise = U.unsafeIndex cs pv


-------------------------------------------------------------------------------
-- Utils
-------------------------------------------------------------------------------
rep :: Monad m => Int -> (Int -> m ()) -> m ()
rep !n = U.forM_ $ U.generate n id
{-# INLINE rep #-}
rev :: Monad m => Int -> (Int -> m ()) -> m ()
rev !n = U.forM_ $ U.iterateN n (subtract 1) (n - 1)
{-# INLINE rev #-}
infixl 8 `shiftRL`, `unsafeShiftRL`
shiftRL = unsafeShiftRL
{-# INLINE shiftRL #-}
unsafeShiftRL (I# x#) (I# i#) = I# (uncheckedIShiftRL# x# i#)
{-# INLINE unsafeShiftRL #-}
type Parser a = StateT C.ByteString Maybe a
runParser :: Parser a -> C.ByteString -> Maybe (a, C.ByteString)
runParser = runStateT
{-# INLINE runParser #-}
int :: Parser Int
int = coerce $ C.readInt . C.dropWhile isSpace
{-# INLINE int #-}
int1 :: Parser Int
int1 = fmap (subtract 1) int
{-# INLINE int1 #-}
char :: Parser Char
char = coerce C.uncons
{-# INLINE char #-}
byte :: Parser Word8
byte = coerce B.uncons
{-# INLINE byte #-}
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
    madj <- UM.unsafeNew numEdgesCSR
    U.forM_ edges $ \(src, dst) -> do
        pos <- UM.unsafeRead moffset src
        UM.unsafeWrite moffset src (pos + 1)
        UM.unsafeWrite madj pos dst
    adjacentCSR <- U.unsafeFreeze madj
    return CSR{edgeCtxCSR = U.replicate numEdgesCSR (), ..}

buildUndirectedGraph :: Int -> U.Vector Edge -> SparseGraph ()
buildUndirectedGraph numVerticesCSR edges = runST $ do
    let numEdgesCSR = 2 * U.length edges
    outDeg <- UM.replicate numVerticesCSR (0 :: Int)
    U.forM_ edges $ \(x, y) -> do
        UM.unsafeModify outDeg (+1) x
        UM.unsafeModify outDeg (+1) y
    offsetCSR <- U.scanl' (+) 0 <$> U.unsafeFreeze outDeg
    moffset <- U.thaw offsetCSR
    madj <- UM.unsafeNew numEdgesCSR
    U.forM_ edges $ \(x, y) -> do
        posX <- UM.unsafeRead moffset x
        posY <- UM.unsafeRead moffset y
        UM.unsafeWrite moffset x (posX + 1)
        UM.unsafeWrite moffset y (posY + 1)
        UM.unsafeWrite madj posX y
        UM.unsafeWrite madj posY x
    adjacentCSR <- U.unsafeFreeze madj
    return CSR{edgeCtxCSR = U.replicate numEdgesCSR (), ..}

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
