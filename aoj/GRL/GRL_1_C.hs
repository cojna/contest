{-# OPTIONS_GHC -O2 -funbox-strict-fields #-}
{-# LANGUAGE BangPatterns #-}

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import qualified Data.ByteString.Char8       as B
import qualified Data.ByteString.Unsafe      as B
import           Data.Char
import           Data.Int
import qualified Data.Vector                 as V
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM

main :: IO ()
main = do
    [numV, numE] <- map read.words <$> getLine :: IO [Int]
    gr <- toDenseGraph numV
        . U.map (\(src,dst,cost) -> (src,dst,fromIntegral cost))
        . U.unfoldrN numE (readInt3.B.dropWhile isSpace)
        <$> B.getContents
    putStr . maybe "NEGATIVE CYCLE\n" showDenseGraph $ solve gr

solve :: DenseGraph -> Maybe DenseGraph
solve gr
    | U.any (<0) diag = Nothing
    | otherwise = Just dists
  where
    dists = warshallFloyd gr
    diag = U.generate (V.length gr) (\i -> unsafeIndex2 dists i i)

type Cost = Int64
type DenseGraph = V.Vector (U.Vector Cost)
type DenseGraphM m = V.Vector (UM.MVector m Cost)

toDenseGraph :: Int -> U.Vector (Int, Int, Cost) -> DenseGraph
toDenseGraph numV edges = runST $ do
    mgr <- V.replicateM numV $
        UM.replicate numV inf
    U.forM_ edges $ \(src, dst, cost) ->
        unsafeWrite2 mgr src dst cost
    V.mapM U.unsafeFreeze mgr

showDenseGraph :: DenseGraph -> String
showDenseGraph gr = unlines
    . map (unwords . map showElem . U.toList)
    $ V.toList gr
  where
    showElem cost
        | cost < inf2 = show cost
        | otherwise = "INF"

inf, inf2 :: Cost
inf = 0xffffffff
inf2 = 0x7fffffff

warshallFloyd :: DenseGraph -> DenseGraph
warshallFloyd gr = runST $ do
    mgr <- V.mapM U.unsafeThaw gr
    warshallFloydM mgr
    V.mapM U.unsafeFreeze mgr

warshallFloydM :: PrimMonad m => DenseGraphM (PrimState m) -> m ()
warshallFloydM mgr = do
    let numV = V.length mgr
    rep numV $ \v ->
        unsafeWrite2 mgr v v 0
    rep numV $ \via ->
        rep numV $ \src ->
            rep numV $ \dst -> do
                old <- unsafeRead2 mgr src dst
                new <- liftM2 (+)
                    (unsafeRead2 mgr src via)
                    (unsafeRead2 mgr via dst)
                when (new < old) $
                    unsafeWrite2 mgr src dst new

readInt3 :: B.ByteString -> Maybe ((Int,Int,Int), B.ByteString)
readInt3 bs = Just ((x,y,z),bsz)
  where
    Just (x, bsx) = B.readInt bs
    Just (y, bsy) = B.readInt $ B.unsafeTail bsx
    Just (z, bsz) = B.readInt $ B.unsafeTail bsy

unsafeModify :: (PrimMonad m, GM.MVector mv a)
             => mv (PrimState m) a -> Int -> (a -> a) -> m ()
unsafeModify v i f = GM.unsafeRead v i >>= GM.unsafeWrite v i . f
{-# INLINE unsafeModify #-}            

unsafeIndex2 :: (G.Vector u a, G.Vector v (u a))
             => v (u a) -> Int -> Int -> a
unsafeIndex2 vv i j = G.unsafeIndex (G.unsafeIndex vv i) j
{-# INLINE unsafeIndex2 #-}

unsafeRead2 :: (PrimMonad m, GM.MVector mu a, G.Vector v (mu (PrimState m) a))
            => v (mu (PrimState m) a)
            -> Int -> Int -> m a
unsafeRead2 vv i j = GM.unsafeRead (G.unsafeIndex vv i) j
{-# INLINE unsafeRead2 #-}

unsafeWrite2 :: (PrimMonad m, GM.MVector mu a, G.Vector v (mu (PrimState m) a))
             => v (mu (PrimState m) a)
             -> Int -> Int -> a -> m ()
unsafeWrite2 vv i j x = GM.unsafeWrite (G.unsafeIndex vv i) j x
{-# INLINE unsafeWrite2 #-}

unsafeModify2 :: (PrimMonad m, GM.MVector mu a, G.Vector v (mu (PrimState m) a))
             => v (mu (PrimState m) a)
             -> Int -> Int -> (a -> a) -> m ()
unsafeModify2 vv i j f = unsafeRead2 vv i j >>= unsafeWrite2 vv i j . f
{-# INLINE unsafeModify2 #-}

-------------------------------------------------------------------------------
rep, rev :: Monad m => Int -> (Int -> m ()) -> m ()
rep !n f = U.mapM_ f $ U.generate n id
rev !n f = U.mapM_ f $ U.iterateN n (subtract 1) (n - 1)
for :: Monad m => Int -> Int -> (Int -> m ()) -> m ()
for !s !t f = U.mapM_ f $ U.generate (t - s) (+s)
{-# INLINE rep #-}
{-# INLINE rev #-}
{-# INLINE for #-}
