{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import qualified Data.ByteString.Char8       as C
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
import           Unsafe.Coerce

main :: IO ()
main = do
    [x, y, z, k] <- map read.words <$> getLine :: IO [Int]
    xs <- U.unfoldrN x (C.readInt.C.dropWhile isSpace) <$> C.getLine
    ys <- U.unfoldrN y (C.readInt.C.dropWhile isSpace) <$> C.getLine
    zs <- U.unfoldrN z (C.readInt.C.dropWhile isSpace) <$> C.getLine
    putStr.unlines.map show $ solve k x y z xs ys zs

type IntIntInt = Int

data Node = Node !Int !IntIntInt deriving (Eq, Ord, Show)

encode :: Int -> Int -> Int -> IntIntInt
encode x y z = unsafeShiftL x 20 .|. unsafeShiftL y 10 .|. z

decode :: IntIntInt -> (Int, Int, Int)
decode xyz = (x, y, z)
  where
    !x = unsafeShiftR xyz 20 .&. 0x3ff
    !y = unsafeShiftR xyz 10 .&. 0x3ff
    !z = xyz .&. 0x3ff

succX :: IntIntInt -> IntIntInt
succX = (+ unsafeShiftL 1 20)

succY :: IntIntInt -> IntIntInt
succY = (+ unsafeShiftL 1 10)

succZ :: IntIntInt -> IntIntInt
succZ = (+ 1)

solve :: Int -> Int -> Int -> Int -> U.Vector Int -> U.Vector Int -> U.Vector Int -> [Int]
solve k x y z
        (U.reverse.radixSort64 -> xs)
        (U.reverse.radixSort64 -> ys)
        (U.reverse.radixSort64 -> zs)
    = take k . go IS.empty $ _HHsingleton (node $ encode 0 0 0)
  where
    priority (decode -> (i, j, k))
        | i < x, j < y, k < z = U.unsafeIndex xs i + U.unsafeIndex ys j + U.unsafeIndex zs k
        | otherwise = minBound
    node ijk = Node (priority ijk) ijk

    go !visited !set = case _HHdeleteFindMax set of
        Just (Node p ijk, set') -> p : go visited' set''
          where
            nexts = filter (flip IS.notMember visited)
                $ map ($ ijk) [succX, succY, succZ]
            visited' = F.foldl' (flip IS.insert) visited nexts
            set'' = F.foldl' (flip _HHinsert) set' $ map node nexts
        Nothing -> []

-------------------------------------------------------------------------------

data MaxHeap a = MaxFork !a [MaxHeap a] | MaxEmpty

_HHempty :: MaxHeap a
_HHempty = MaxEmpty
{-# INLINE _HHempty #-}

_HHsingleton :: a -> MaxHeap a
_HHsingleton x = MaxFork x []
{-# INLINE _HHsingleton #-}

_HHnull :: MaxHeap a -> Bool
_HHnull MaxEmpty      = True
_HHnull (MaxFork _ _) = False
{-# INLINE _HHnull #-}

_HHinsert :: Ord a => a -> MaxHeap a -> MaxHeap a
_HHinsert x = _HHmerge (MaxFork x [])
{-# INLINE _HHinsert #-}

_HHMaxElem :: MaxHeap a -> Maybe a
_HHMaxElem (MaxFork x _) = Just x
_HHMaxElem MaxEmpty      = Nothing
{-# INLINE _HHMaxElem #-}

_HHdeleteMax :: Ord a => MaxHeap a -> Maybe (MaxHeap a)
_HHdeleteMax (MaxFork _ hs) = Just $ _HHmergePairs hs
_HHdeleteMax MaxEmpty       = Nothing
{-# INLINE _HHdeleteMax #-}

_HHdeleteFindMax :: Ord a => MaxHeap a -> Maybe (a, MaxHeap a)
_HHdeleteFindMax (MaxFork x hs) = Just (x, _HHmergePairs hs)
_HHdeleteFindMax MaxEmpty       = Nothing
{-# INLINE _HHdeleteFindMax #-}

_HHmerge :: Ord a => MaxHeap a -> MaxHeap a -> MaxHeap a
_HHmerge hx@(MaxFork x hxs) hy@(MaxFork y hys)
  | y <= x    = MaxFork x (hy:hxs)
  | otherwise = MaxFork y (hx:hys)
_HHmerge MaxEmpty hy = hy
_HHmerge hx _ = hx
{-# INLINE _HHmerge #-}

_HHmergePairs :: Ord a => [MaxHeap a] -> MaxHeap a
_HHmergePairs (x:y:hs) = (x <> y) <> _HHmergePairs hs
_HHmergePairs [x]      = x
_HHmergePairs []       = MaxEmpty
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
    mconcat = _HHmergePairs
    {-# INLINE mconcat #-}
    mappend = _HHmerge
    {-# INLINE mappend #-}


radixSort64 :: U.Vector Int -> U.Vector Int
radixSort64 v = F.foldl' step v [0, 16, 32, 48]
  where
    mask k x = fromIntegral $ unsafeShiftR x k .&. 0xffff
    step v k = U.create $ do
        pref <- U.unsafeThaw
            . U.prescanl' (+) 0
            . U.unsafeAccumulate (+) (U.replicate 0x10000 0)
            $ U.map (flip (,) 1 . mask k) v
        res <- UM.unsafeNew $ U.length v
        U.forM_ v $ \x -> do
            let !masked = mask k x
            i <- UM.unsafeRead pref masked
            UM.unsafeWrite pref masked $ i + 1
            UM.unsafeWrite res i x
        return res
{-# INLINE radixSort64 #-}

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