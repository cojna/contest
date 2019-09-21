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
    [n,m] <- map read.words <$> getLine :: IO [Int]
    xs <- U.unfoldrN n parseInt <$> C.getLine
    print $ solve n m xs

solve :: Int -> Int -> U.Vector Int -> Int
solve n m xs = go m . fromList $ U.toList xs
  where
    go 0 !h = sum $ toList h
    go !acc !h = case _HHdeleteFindMax h of
        Just (x, h') -> go (acc - 1) $ _HHinsert (quot x 2) h'
        Nothing -> undefined

-------------------------------------------------------------------------------

data MaxHeap a = MaxFork !a [MaxHeap a] | MaxEmpty

_HHempty :: MaxHeap a
_HHempty = MaxEmpty
{-# INLINE _HHempty #-}

_HHsingleton :: a -> MaxHeap a
_HHsingleton = flip MaxFork []
{-# INLINE _HHsingleton #-}

_HHnull :: MaxHeap a -> Bool
_HHnull (MaxFork _ _) = False
_HHnull MaxEmpty      = True
{-# INLINE _HHnull #-}

_HHinsert :: Ord a => a -> MaxHeap a -> MaxHeap a
_HHinsert = _HHmerge . _HHsingleton
{-# INLINE _HHinsert #-}

_HHMaxElem :: MaxHeap a -> Maybe a
_HHMaxElem (MaxFork x _) = Just x
_HHMaxElem MaxEmpty      = Nothing
{-# INLINE _HHMaxElem #-}

_HHdeleteMax :: Ord a => MaxHeap a -> Maybe (MaxHeap a)
_HHdeleteMax (MaxFork _ hs) = Just $! _HHmergePairs hs
_HHdeleteMax MaxEmpty       = Nothing
{-# INLINE _HHdeleteMax #-}

_HHdeleteFindMax :: Ord a => MaxHeap a -> Maybe (a, MaxHeap a)
_HHdeleteFindMax (MaxFork x hs) = case _HHmergePairs hs of
    merged -> Just $! (x, merged)
_HHdeleteFindMax MaxEmpty       = Nothing
{-# INLINE _HHdeleteFindMax #-}

_HHmerge :: Ord a => MaxHeap a -> MaxHeap a -> MaxHeap a
_HHmerge hx@(MaxFork x hxs) hy@(MaxFork y hys)
  | y <= x    = MaxFork x (hy:hxs)
  | otherwise = MaxFork y (hx:hys)
_HHmerge MaxEmpty hy = hy
_HHmerge hx MaxEmpty = hx
{-# INLINE _HHmerge #-}

_HHmergePairs :: Ord a => [MaxHeap a] -> MaxHeap a
_HHmergePairs = mconcat . mergePairs
  where
    mergePairs (x:y:xs) = case x <> y of
        merged -> merged : mergePairs xs
    mergePairs xs = xs
{-# INLINE _HHmergePairs #-}

instance Ord a => Eq (MaxHeap a) where
    (==) = (==) `on` toList

instance Ord a => Ord (MaxHeap a) where
    compare = compare `on` toList

instance Ord a => IsList (MaxHeap a) where
    type Item (MaxHeap a) = a
    fromList xs = _HHmergePairs $ map _HHsingleton xs
    toList = L.unfoldr _HHdeleteFindMax

instance (Show a, Ord a) => Show (MaxHeap a) where
    show = show . toList

instance Ord a => Monoid (MaxHeap a) where
    mempty = _HHempty
    {-# INLINE mempty #-}
    mappend = _HHmerge
    {-# INLINE mappend #-}


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