{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}

import           Control.Applicative
import           Control.DeepSeq
import           Control.Exception
import           Control.Monad
import           Control.Monad.ST
import           Control.Monad.Trans.State.Strict
import           Data.Array.Base
import           Data.Array.ST                    (STUArray, runSTUArray)
import           Data.Bits
import           Data.Bool
import qualified Data.ByteString.Char8            as B
import           Data.Char
import           Data.Coerce
import qualified Data.Foldable                    as F
import           Data.Function
import           Data.Int
import qualified Data.IntMap.Strict               as IM
import qualified Data.IntSet                      as IS
import qualified Data.List                        as L
import qualified Data.Map.Strict                  as M
import           Data.Monoid
import           Data.Ord
import           Data.Ratio
import qualified Data.Set                         as S
import           Data.STRef
import           Data.Tuple
import           Data.Word
import           Debug.Trace
import           GHC.Arr                          (Array, Ix (..), STArray)
import           GHC.Exts
import           System.Exit
import           System.IO


main :: IO ()
main = runGCJ $ do
    [n, l] <- map read.words <$> getLine
    xs <- L.unfoldr (runStateT parseInt) <$> B.getLine
    print $ solve n xs

inf :: Int
inf = 0x3f3f3f3f

solve :: Int -> [Int] -> Int
solve n xs = go 0 rest . fromList . map (\x -> (priority x, x)) $ xs ++ replicate rest 0
  where
    !rest = n - sum xs
    go !score !rest !heap = case deleteFindMin heap of
        Just ((p, x), h)
            | dx <= rest -> go (score + calc nx) (rest - dx) $ insert (priority nx, nx) h
            | otherwise -> sum . map (calc.snd) $ toList heap
          where
            !nx = next x
            !dx = next x - x
        Nothing -> undefined

    priority :: Int -> Rational
    priority x
        | r < 1 % 2 = 1 % 2 - r
        | otherwise = fromIntegral inf % 1
      where
        p = fromIntegral x * 100 % fromIntegral n
        r = p - floor p % 1

    next x = lowerBound 0 512 $ \i -> calc i > fx
     where
       !fx = calc x

    calc x = ((x * 1000) `div` n + 5) `div` 10

-------------------------------------------------------------------------------
data Heap a = Fork !a [Heap a] | Empty

singleton :: a -> Heap a
singleton x = Fork x []
{-# INLINE singleton #-}

isEmpty :: Heap a -> Bool
isEmpty Empty = True
isEmpty _     = False
{-# INLINE isEmpty #-}

insert :: Ord a => a -> Heap a -> Heap a
insert x = merge (Fork x [])
{-# INLINE insert #-}

minElem :: Heap a -> Maybe a
minElem (Fork x _) = Just x
minElem _          = Nothing
{-# INLINE minElem #-}

deleteMin :: Ord a => Heap a -> Maybe (Heap a)
deleteMin (Fork _ hs) = Just $! mergePairs hs
deleteMin _           = Nothing
{-# INLINE deleteMin #-}

deleteFindMin :: Ord a => Heap a -> Maybe (a, Heap a)
deleteFindMin (Fork x hs) = Just (x, mergePairs hs)
deleteFindMin _           = Nothing
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
    toList = L.unfoldr deleteFindMin

instance (Show a, Ord a) => Show (Heap a) where
    show = show . toList

lowerBound :: (Integral i) => i -> i -> (i -> Bool) -> i
lowerBound low high p = assert (low >= 0 && p high) $ go low high
  where
    go !low !high
        | high <= low = high
        | p mid       = go low       mid
        | otherwise   = go (mid + 1) high
      where
        mid = low + (high - low) `quot` 2
{-# INLINE lowerBound #-}

type Parser a = StateT B.ByteString Maybe a

parseInt :: Parser Int
parseInt = coerce $ B.readInt . B.dropWhile isSpace

parseInt2 :: Parser (Int, Int)
parseInt2 = (,) <$> parseInt <*> parseInt

rep, rev :: Applicative f => Int -> (Int -> f ()) -> f ()
rep n f=F.traverse_ f[0..n-1]
rev n f=F.traverse_(f.negate)[1-n..0]
for :: Applicative f => Int -> Int -> (Int -> f ()) -> f ()
for a b f=F.traverse_ f[a..b]
{-# INLINE rep #-}
{-# INLINE rev #-}
{-# INLINE for #-}

modifyArray :: (MArray a e m, Ix i) => a i e -> i -> (e -> e) -> m ()
modifyArray a i f=readArray a i>>=writeArray a i.f
{-# INLINE modifyArray #-}
unsafeModify :: (MArray a e m, Ix i) => a i e -> Int -> (e -> e) -> m ()
unsafeModify a i f=unsafeRead a i>>=unsafeWrite a i.f
{-# INLINE unsafeModify #-}

runGCJ :: IO () -> IO ()
runGCJ main_ = do
    !t <- readLn :: IO Int
    F.for_ [1..t] $ \i -> do
        putStr $ "Case #" ++ shows i ": "
        main_

