{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns #-}

import           Control.Applicative
import           Control.Monad
import           Control.Monad.ST
import           Data.Bits
import qualified Data.ByteString.Char8       as B
import qualified Data.ByteString.Unsafe      as B
import           Data.Char
import           Data.Function
import           Data.Tuple
import qualified Data.Vector                 as V
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM
import           Data.Word
import           Unsafe.Coerce

main :: IO ()
main = do
    n <- readLn
    tree <- V.replicateM n $
        U.unsafeTail.U.unfoldr (B.readInt.B.dropWhile isSpace) <$> B.getLine
    q <- readLn
    qs <- U.unfoldrN q (readInt2.B.dropWhile isSpace) <$> B.getContents
    putStr.unlines.map show.U.toList $ solve tree qs

solve :: Graph -> U.Vector (Int, Int) -> U.Vector Int
solve gr = U.map (uncurry $ queryLCA lca)
  where
    !lca = buildLCA gr

readInt2 :: B.ByteString -> Maybe ((Int,Int), B.ByteString)
readInt2 bs = Just ((x,y),bsy)
  where
    Just (x, bsx) = B.readInt bs
    Just (y, bsy) = B.readInt $ B.unsafeTail bsx

floorLog2 :: Int -> Int
floorLog2 x = fromIntegral $ unsafeShiftR y 52 - 1023
  where
    y :: Word64
    y = unsafeCoerce (fromIntegral x :: Double)

unsafeIndex2 :: (G.Vector u a, G.Vector v (u a))
             => v (u a) -> Int -> Int -> a
unsafeIndex2 vv i j = G.unsafeIndex (G.unsafeIndex vv i) j
{-# INLINE unsafeIndex2 #-}

type Vertex = Int
type Graph = V.Vector (U.Vector Vertex)

newtype RMQ a = RMQ {rmqTable :: V.Vector (U.Vector (a, Int))}

buildRMQ :: (U.Unbox a, Ord a) => U.Vector a -> RMQ a
buildRMQ vec = RMQ
    . V.scanl' (\acc i -> U.zipWith min acc $ U.drop i acc) veci
    $ V.iterateN (floorLog2 $ U.length vec) (*2) 1
  where
    veci = U.map swap $ U.indexed vec

queryMin :: (U.Unbox a, Ord a) => RMQ a -> Int -> Int -> a
queryMin rmq l r
    | l < r = fst $ min x y
    | l == r = fst $ unsafeIndex2 (rmqTable rmq) 0 l
    | otherwise = error "queryMin"
  where
    lg = floorLog2 $ r - l
    x = unsafeIndex2 (rmqTable rmq) lg l
    y = unsafeIndex2 (rmqTable rmq) lg $ r - unsafeShiftL 1 lg + 1
{-# INLINE queryMin #-}

queryMinIndex :: (U.Unbox a, Ord a) => RMQ a -> Int -> Int -> Int
queryMinIndex rmq l r
    | l < r = snd $ min x y
    | l == r = snd $ unsafeIndex2 (rmqTable rmq) 0 l
    | otherwise = error "queryMinIndex"
  where
    lg = floorLog2 $ r - l
    x = unsafeIndex2 (rmqTable rmq) lg l
    y = unsafeIndex2 (rmqTable rmq) lg $ r - unsafeShiftL 1 lg + 1
{-# INLINE queryMinIndex #-}

nothing :: Int
nothing = -1

data LCA = LCA (U.Vector Vertex) (U.Vector Int) (RMQ Vertex)

buildLCA :: Graph -> LCA
buildLCA tree =  runST $ do
    eulertour <- UM.replicate (2 * V.length tree - 1) nothing
    idx <-UM.replicate (V.length tree) nothing
    depth <- UM.replicate (2 * V.length tree - 1) nothing

    void $ fix `flip` 0 `flip` nothing `flip` 0 `flip` 0 $ \dfs v p d i -> do
        UM.unsafeWrite eulertour i v
        UM.unsafeWrite idx v i
        UM.unsafeWrite depth i d

        U.foldM' `flip` (i + 1) `flip` V.unsafeIndex tree v $ \j u ->
            if u == p
            then return j
            else do
                k <- dfs u v (d + 1) j
                UM.unsafeWrite eulertour k v
                UM.unsafeWrite depth k d
                return $ k + 1

    LCA <$> U.unsafeFreeze eulertour
        <*> U.unsafeFreeze idx
        <*> (buildRMQ <$> U.unsafeFreeze depth)

queryLCA :: LCA -> Vertex -> Vertex -> Vertex
queryLCA (LCA eulertour idx rmq) v u = U.unsafeIndex eulertour
    $ queryMinIndex rmq (min i j) (max i j)
  where
    !i = U.unsafeIndex idx v
    !j = U.unsafeIndex idx u
{-# INLINE queryLCA #-}