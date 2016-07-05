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
import           Data.List hiding (insert)
import qualified Data.Map.Strict             as M
import           Data.Monoid
import           Data.Ord
import           Data.Primitive.MutVar
import qualified Data.Set                    as S
import           Data.Tuple
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import qualified Data.Vector                 as V
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM
import           Debug.Trace
import           Foreign                     hiding (void)
import           System.IO
import           Unsafe.Coerce


solve :: Task -> Answer
solve ns = go (sum ns).mergePairs.map singleton $ zip ns ['A'..]
  where
    go _ Empty = []
    go total heap
      | isValid (total-1) h' = [c]:go (total-1) h'
      | Just ((n',c'),h'')<-deleteFindMax h' = [c,c']: go (total-2) (if n'-1 > 0 then insert (n'-1,c') h'' else h'')
      where
        Just ((n,c),h) = deleteFindMax heap
        h' = if n-1 > 0 then insert (n-1,c) h else h

isValid total heap = case maxElem heap of
  Just (n,_)
    | 2 * n > total -> False
    | otherwise -> True
  Nothing -> True

type Task = [Int]
getTask :: IO Task
getTask = do
    _ <- getLine
    map read.words <$> getLine

type Answer = [String]
putAnswer :: Answer -> IO ()
putAnswer = putStrLn . unwords

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

data Heap a = Fork !a [Heap a] | Empty deriving Show

singleton :: a -> Heap a
singleton x = Fork x []
{-# INLINE singleton #-}

isEmpty :: Heap a -> Bool
isEmpty Empty = True
isEmpty _ = False
{-# INLINE isEmpty #-}

insert :: Ord a => a -> Heap a -> Heap a
insert x = merge (Fork x [])
{-# INLINE insert #-}

maxElem :: Heap a -> Maybe a
maxElem (Fork x _) = Just x
maxElem _ = Nothing
{-# INLINE maxElem #-}

deleteMax :: Ord a => Heap a -> Maybe (Heap a)
deleteMax (Fork _ hs) = Just $ mergePairs hs
deleteMax _ = Nothing
{-# INLINE deleteMax #-}

deleteFindMax :: Ord a => Heap a -> Maybe (a, Heap a)
deleteFindMax (Fork x hs) = Just (x, mergePairs hs)
deleteFindMax _ = Nothing
{-# INLINE deleteFindMax #-}

merge :: Ord a => Heap a -> Heap a -> Heap a
merge hx@(Fork x hxs) hy@(Fork y hys)
  | x >= y    = Fork x (hy:hxs)
  | otherwise = Fork y (hx:hys)
merge Empty hy = hy
merge hx _ = hx
{-# INLINE merge #-}

mergePairs :: Ord a => [Heap a] -> Heap a
mergePairs (x:y:hs) = merge (merge x y) (mergePairs hs)
mergePairs [x]      = x
mergePairs []       = Empty
{-# INLINE mergePairs #-}