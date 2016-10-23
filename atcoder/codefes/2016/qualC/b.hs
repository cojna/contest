{-# LANGUAGE TypeFamilies #-}

import           Control.Applicative
import           GHC.Exts

main :: IO ()
main = do
  [k, t] <- map read.words <$> getLine
  xs <- map read.words <$> getLine
  print $ solve k $ zip xs [0..]

solve :: Int -> [(Int, Int)] -> Int
solve k xis = go (-1) $ fromList xis
  where
    go :: Int -> Heap(Int, Int) -> Int
    go l heap
      | Just ((x, i), h') <- deleteFindMax heap
      , Just ((y, j), h'') <- deleteFindMax h' = go j $ ins (x-1, i) $ ins (y-1, j) h''
      | Just ((x, i), _) <- deleteFindMax heap = if i == l
          then x
          else max 0 $ x - 1
      | otherwise = 0


ins (0, _) heap = heap
ins x heap = insert x heap

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

instance Ord a => IsList (Heap a) where
    type Item (Heap a) = a
    fromList xs = mergePairs $ map singleton xs
    toList (Fork x hs) = x : toList (mergePairs hs)
    toList Empty = []
