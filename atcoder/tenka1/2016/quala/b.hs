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
import           Data.Bool
import qualified Data.ByteString.Char8       as B
import qualified Data.ByteString.Unsafe      as B
import           Data.Char
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
import qualified Data.Vector                 as V
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM
import           Debug.Trace
import           Foreign                     hiding (void)
import           Unsafe.Coerce

main :: IO ()
main = do
    [n, m] <- map read.words <$> getLine :: IO [Int]
    ps <- U.fromList <$> replicateM (n-1) readLn :: IO (U.Vector Int)
    bcs <- U.unfoldrN m parseInt2 <$> B.getContents
    print $ solve n ps bcs

solve :: Int -> U.Vector Int -> U.Vector (Int, Int) -> Int
solve n ps bcs = U.sum $ U.map (\(p, c)->cs U.! c - cs U.! p + sum[cs U.! p|p==0]) edges
    where
      !gr = fromEdges n edges
      !edges = U.map (fmap (+1). swap) $ U.indexed ps
      cs = U.create $ do
          cost <- UM.replicate n (-1)

          U.forM_ bcs $ \(b, c) -> do
              UM.write cost b c

          let memo v = do
              c <- UM.read cost v
              if c >= 0
              then return c
              else do
                  c' <- U.minimum <$> U.mapM memo (gr V.! v)
                  UM.write cost v c'
                  return c'
          _ <- memo 0
          return cost


-------------------------------------------------------------------------------bn
type Vertex = Int
type Edge = (Vertex, Vertex)
type Graph = V.Vector (U.Vector Vertex)

fromEdges :: Int -> U.Vector Edge -> Graph
fromEdges numV edges = V.map U.fromList
    . V.unsafeAccumulate (flip (:)) (V.replicate numV [])
    . U.convert

    $ edges
type Parser a = B.ByteString -> Maybe (a, B.ByteString)

parseInt :: Parser Int
parseInt = B.readInt . B.dropWhile isSpace

parseInt2 :: Parser (Int, Int)
parseInt2 = runStateT $
    (,) <$> StateT (B.readInt . B.dropWhile isSpace)
        <*> StateT (B.readInt . B.unsafeTail)