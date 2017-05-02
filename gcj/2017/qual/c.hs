{-# LANGUAGE BangPatterns #-}

import           Control.Monad
import           Data.Fixed
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Debug.Trace

main :: IO ()
main = runGCJ $ do
    [n, k] <- map read.words <$> getLine
    let (m, mm) = solveSmall n k
    putStrLn.unwords.map show$[mm, m]

solveSmall :: Int -> Int -> (Int, Int)
solveSmall n k = go k $ singleton (n, 0)
  where
    go !rest heap = case deleteFindMax heap of
        Just ((x, i), h)
          | rest == 1 -> dist x
          | (m, mm) <- dist x -> go (rest - 1)
              . insert (m, i)
              $ insert (mm, i - mm) h
        Nothing -> (0, 0)

dist :: Int -> (Int, Int)
dist s
  | even s = (q - 1, q)
  | otherwise = (q, q)
  where
    q = div s 2



data Heap a = Fork !a [Heap a] | Empty

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

runGCJ :: IO () -> IO ()
runGCJ main_ = do
    t <- readLn
    forM_ [1..t] $ \i -> do
        putStr $ "Case #" ++ shows i ": "
        traceIO $ shows i "/" ++ shows t ": "
        time <- flip (-) <$> getPOSIXTime <* main_ <*> getPOSIXTime
        traceIO $ (shows.msec) time "ms\n"

msec :: NominalDiffTime -> Int
msec s = let t = realToFrac s :: Milli in fromEnum t
