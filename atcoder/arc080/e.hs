{-# OPTIONS_GHC -O2 -funbox-strict-fields #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}

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
import           Data.List hiding (insert)
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
import           Unsafe.Coerce

main :: IO ()
main = do
    !n <- readLn :: IO Int
    xs <- U.unfoldrN n (B.readInt.B.dropWhile isSpace) <$> B.getLine
    putStrLn.unwords.map show $ solve n xs

solve :: Int -> U.Vector Int -> [Int]
solve n xs = map (U.unsafeIndex xs).go $ singleton (priority 0 (n - 1), 0, n - 1)
  where
    !rmq0 = buildRMQ.U.map(\(i,x) -> (if even i then x else maxBound))$ U.indexed xs
    !rmq1 = buildRMQ.U.map(\(i,x) -> (if odd i then x else maxBound))$ U.indexed xs
    go heap = case deleteFindMin heap of
        Just ((_, l0, r0), h)
          | r0 - l0 == 1 -> l0:r0:go h
          | (l, r) <- split (even l0) l0 r0
              -> (l:).(r:)
                .go
                .foldl' (flip insert) h
                . map (\(x, y) -> (priority x y, x, y))
                $ filter(uncurry(<))[(l0, l-1), (l+1, r-1), (r+1, r0)]
        Nothing -> []

    priority l r
      | even l = queryMin rmq0 l r
      | otherwise = queryMin rmq1 l r

    split True l0 r0 = (l, r)
      where
        l = queryMinIndex rmq0 l0 r0
        r = queryMinIndex rmq1 l r0
    split False l0 r0 = (l, r)
      where
        l = queryMinIndex rmq1 l0 r0
        r = queryMinIndex rmq0 l r0

-------------------------------------------------------------------------------
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

minElem :: Heap a -> Maybe a
minElem (Fork x _) = Just x
minElem _ = Nothing
{-# INLINE minElem #-}

deleteMin :: Ord a => Heap a -> Maybe (Heap a)
deleteMin (Fork _ hs) = Just $! mergePairs hs
deleteMin _ = Nothing
{-# INLINE deleteMin #-}

deleteFindMin :: Ord a => Heap a -> Maybe (a, Heap a)
deleteFindMin (Fork x hs) = Just (x, mergePairs hs)
deleteFindMin _ = Nothing
{-# INLINE deleteFindMin #-}

merge :: Ord a => Heap a -> Heap a -> Heap a
merge hx@(Fork x hxs) hy@(Fork y hys)
  | x <= y    = Fork x (hy:hxs)
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

newtype RMQ a = RMQ {rmqTable :: V.Vector (U.Vector (a, Int))}

buildRMQ :: (U.Unbox a, Ord a) => U.Vector a -> RMQ a
buildRMQ vec = RMQ
    . V.scanl' (\acc i -> U.zipWith min acc $ U.drop i acc) veci
    $ V.iterateN (floorLog2 $ U.length vec) (*2) 1
  where
    veci = U.map swap $ U.indexed vec

queryMin :: (U.Unbox a, Ord a) => RMQ a -> Int -> Int -> a
queryMin rmq l r
    | l == r = fst $ unsafeIndex2 (rmqTable rmq) 0 l
    | l < r = fst $ min x y
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

unsafeIndex2 :: (G.Vector u a, G.Vector v (u a))
             => v (u a) -> Int -> Int -> a
unsafeIndex2 vv i = G.unsafeIndex (G.unsafeIndex vv i)
{-# INLINE unsafeIndex2 #-}

floorLog2 :: Int -> Int
floorLog2 x = fromIntegral $ unsafeShiftR y 52 - 1023
  where
    y :: Word64
    y = unsafeCoerce (fromIntegral x :: Double)
