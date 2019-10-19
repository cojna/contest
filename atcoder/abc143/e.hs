{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns, CPP, LambdaCase, MagicHash                   #-}
{-# LANGUAGE MultiParamTypeClasses, MultiWayIf, OverloadedStrings       #-}
{-# LANGUAGE RecordWildCards, TupleSections, TypeFamilies, ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

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
    [n, m, l] <- map read.words <$> getLine :: IO [Int]
    vec <- U.unfoldr parseInt <$> C.getContents
    let (!abcs, !vec') = U.splitAt (3 * m) vec
    let !q = U.head vec'
    putStr.unlines.map show.U.toList
        $ solve n m l (triplesN m abcs) q (pairsN q $ U.tail vec')

inf :: Int
inf = 0x3f3f3f3f3f3f3f3f

solve :: Int -> Int -> Int -> U.Vector (Int, Int, Int) -> Int -> U.Vector (Int, Int) -> U.Vector Int
solve n m l abcs q qs = U.map query qs
  where
    query (s, t)
        | d < inf = d - 1
        | otherwise = -1
      where
        !d = dist U.! ((s - 1) * n + t - 1)
    !dist = U.create $ do
        gr <- UM.replicate (n * n) inf
        let ix i j = i * n + j
        rep n $ \i -> do
            UM.unsafeWrite gr (ix i i) 0
        U.forM_ abcs $ \(a0,b0,c) -> do
            let a = a0 - 1
            let b = b0 - 1
            -- traceShowM (a,b,c)
            when (c <= l) $ do
                UM.unsafeWrite gr (ix a b) c
                UM.unsafeWrite gr (ix b a) c
        warshallFloyd n gr
        rep (n * n) $ \i -> do
            d <- UM.unsafeRead gr i
            when (d > 0) $ do
                if d <= l
                then UM.unsafeWrite gr i 1
                else UM.unsafeWrite gr i inf
        warshallFloyd n gr
        return gr

-------------------------------------------------------------------------------

pairsN :: (G.Vector v a, G.Vector v (a, a)) => Int -> v a -> v (a, a)
pairsN n = G.unfoldrN n $ \v ->
  case G.length v >= 2 of
    True -> let !x = G.unsafeIndex v 0
                !y = G.unsafeIndex v 1
            in Just ((x, y), G.drop 2 v)
    False -> Nothing

triplesN :: (G.Vector v a, G.Vector v (a, a, a)) => Int -> v a -> v (a, a, a)
triplesN n = G.unfoldrN n $ \v ->
  case G.length v >= 3 of
    True -> let !x = G.unsafeIndex v 0
                !y = G.unsafeIndex v 1
                !z = G.unsafeIndex v 2
            in Just ((x, y, z), G.drop 3 v)
    False -> Nothing

-- | O(V^3)
warshallFloyd :: (PrimMonad m, U.Unbox a, Num a, Ord a)
    => Int -> UM.MVector (PrimState m) a -> m ()
warshallFloyd n d = do
    rep n $ \i -> do
        UM.unsafeWrite d (i * n + i) 0
    rep n $ \k -> do
        rep n $ \i -> do
            rep n $ \j -> do
                dij <- UM.unsafeRead d (i * n + j)
                dik <- UM.unsafeRead d (i * n + k)
                dkj <- UM.unsafeRead d (k * n + j)
                UM.unsafeWrite d (i * n + j) $ min dij (dik + dkj)
{-# INLINE warshallFloyd #-}

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
