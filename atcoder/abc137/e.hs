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
import qualified Data.Set                    as S
import           Data.Tuple
import qualified Data.Vector                 as V
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM
import           Debug.Trace
import           Foreign                     hiding (void)
import           GHC.Exts
import qualified System.IO                   as IO
import           Unsafe.Coerce

main :: IO ()
main = do
    [n, m, p] <- map read.words <$> getLine :: IO [Int]
    edges <- U.unfoldrN m parseInt3 <$> C.getContents
    print $ solve n p edges

solve :: Int -> Int -> U.Vector (Int, Int, Int) -> Int
solve n p edges
    | d == minBound = -1
    | otherwise = max 0 $ negate d
  where
    d = U.last
        . bellmanFord n 0
        $ U.map (\(src, dst, cost) -> (src - 1, dst - 1, p - cost)) edges

type Vertex = Int

bellmanFord :: Int -> Vertex -> U.Vector (Vertex, Vertex, Int) -> U.Vector Int
bellmanFord n root edges = U.create $ do
    dist <- UM.replicate n maxBound
    UM.write dist root 0
    U.replicateM (n - 1) $ do
        U.forM_ edges $ \(src, dst, cost) -> do
            dv <- UM.unsafeRead dist src
            du <- UM.unsafeRead dist dst
            when (dv + cost < du && dv /= maxBound) $ do
                UM.unsafeWrite dist dst $ dv + cost

    U.forM_ edges $ \(src, dst, cost) -> do
        dv <- UM.unsafeRead dist src
        du <- UM.unsafeRead dist dst
        when (dv + cost < du && dv /= maxBound) $ do
            UM.unsafeWrite dist dst minBound

    U.replicateM (n - 1) $ do
        U.forM_ edges $ \(src, dst, _) -> do
            dv <- UM.unsafeRead dist src
            when (dv == minBound) $ do
                UM.unsafeWrite dist dst minBound

    return dist

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