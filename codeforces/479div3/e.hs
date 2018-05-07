{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe              #-}
{-# LANGUAGE TupleSections     #-}

import           Control.Applicative
import           Control.Monad
import           Control.Monad.ST.Safe
import           Control.Monad.Trans.State.Strict
import           Data.Array.IArray
import           Data.Array.ST.Safe
import           Data.Array.Unboxed
import           Data.Bits
import           Data.Bool
import qualified Data.ByteString.Char8            as B
import           Data.Char
import           Data.Function
import           Data.Int
import qualified Data.IntMap.Strict               as IM
import qualified Data.IntSet                      as IS
import           Data.List
import qualified Data.Map.Strict                  as M
import           Data.Monoid
import           Data.Ord
import qualified Data.Set                         as S
import           Data.STRef
import           Data.Tuple
import           Data.Word

main :: IO ()
main = do
    [n, m] <- map read.words <$> getLine
    edges <- unfoldr (runStateT parser) <$> B.getContents
    print $ solve n m edges

parser :: Parser (Int, Int)
parser = on (,) (subtract 1)
    <$> parseInt
    <*> parseInt

solve :: Int -> Int -> [(Int, Int)] -> Int
solve n _ edges = length [vs | vs<-scc gr, all (\v -> length (gr ! v) == 2) vs]
  where
    !gr = mkGraph (0, n - 1) $ concatMap (\(x, y) -> [(x, y), (y, x)]) edges

-------------------------------------------------------------------------------
type Vertex = Int
type Edge = (Vertex, Vertex)
type Graph = Array Vertex [Vertex]
type Path = [Vertex]

mkGraph :: (Int, Int) -> [Edge] -> Graph
mkGraph bnd edges = accumArray (flip (:)) [] bnd edges

vertices :: Graph -> [Vertex]
vertices gr = range $ bounds gr

edges :: Graph -> [Edge]
edges gr = [(from, to) | from <- vertices gr, to <- (!) gr from ]

revGraph :: Graph -> Graph
revGraph gr = mkGraph (bounds gr) . map swap $ edges gr

dfsAll :: Graph -> [Vertex] -> [[Vertex]]
dfsAll gr vs = filter (not.null) $ runST $ do
  vis <- newArray (bounds gr) False :: ST s (STUArray s Vertex Bool)
  mapM (dfsM gr vis) vs

dfsM :: Graph -> STUArray s Vertex Bool -> Vertex -> ST s [Vertex]
dfsM gr vis root = go [] [root]
  where
    go res (v:vs) = do
        visited <- readArray vis v
        if visited then go res vs
        else do
          writeArray vis v True
          go (v:res) ((!) gr v ++ vs)
    go res [] = return $ reverse res

topSort :: Graph -> [Vertex]
topSort gr = runST $ do
  vis   <- newArray (bounds gr) False :: ST s (STUArray s Vertex Bool)
  stack <- newSTRef []                :: ST s (STRef s [Vertex])
  let visit v = do
        visited <- readArray vis v
        unless visited $ do
          writeArray vis v True
          mapM_ visit $ (!) gr v
          modifySTRef stack (v:)
  mapM_ visit $ vertices gr
  readSTRef stack

scc :: Graph -> [[Vertex]]
scc gr = dfsAll gr . topSort $ revGraph gr

type Parser a = StateT B.ByteString Maybe a

parseInt :: Parser Int
parseInt = StateT $ B.readInt . B.dropWhile isSpace

parseInt2 :: Parser (Int, Int)
parseInt2 = (,) <$> parseInt <*> parseInt

parseInt3 :: Parser (Int, Int, Int)
parseInt3 = (,,) <$> parseInt <*> parseInt <*> parseInt
