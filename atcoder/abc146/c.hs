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
    [a, b, x] <- map read.words <$> getLine :: IO [Integer]
    print $ solve a b x

solve a b x
    | a + b > x = 0
    | otherwise = maximum $ do
        d <- [1..10]
        let low = 10^(d-1)
        guard $ a * low + b * d <= x
        let ub = upperBound low (min 1000000000 $ 10^d-1) $ \i ->
                a * i + b * d <= x
        return $! ub

lowerBound :: (Integral i) => i -> i -> (i -> Bool) -> i
lowerBound low high p = assert (p high) $ go low high
  where
    go !low !high
        | high <= low = high
        | p mid       = go low       mid
        | otherwise   = go (mid + 1) high
      where
        h = toInteger high
        l = toInteger low
        mid = fromIntegral $ l + div (h - l) 2
{-# INLINE lowerBound #-}

upperBound :: (Integral i) => i -> i -> (i -> Bool) -> i
upperBound low high p
    | p high = high
    | otherwise = lowerBound low high (not.p) - 1
{-# INLINE upperBound #-}
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

