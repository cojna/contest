{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns, CPP, FlexibleInstances, LambdaCase, MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses, MultiWayIf, OverloadedStrings        #-}
{-# LANGUAGE TupleSections, TypeFamilies                                 #-}

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
    [n, m] <- map read.words <$> getLine :: IO [Int]
    xys <- U.unfoldrN n parseInt2 <$> C.getContents
    print $ solve n m xys

solve :: Int -> Int -> U.Vector (Int, Int) -> Int
solve n m xys = go 0 _Hempty $ U.toList
        . radixSortNonNegative
        . U.map (\(x, y) -> (m - x, y))
        $ U.filter ((<= m) . fst) xys
  where
    go !size !h ((qt, qv):qs)
        | size <= qt = go (size + 1) (_Hinsert qv h) qs
        | size - 1 == qt, Just (x, h') <- _HdeleteFindMin h, x < qv
            = go size (_Hinsert qv h') qs
        | otherwise = go size h qs
    go _ h [] = sum $ toList h

-------------------------------------------------------------------------------

data MinHeap a = MinFork !a [MinHeap a] | MinEmpty

_Hempty :: MinHeap a
_Hempty = MinEmpty
{-# INLINE _Hempty #-}

_Hsingleton :: a -> MinHeap a
_Hsingleton x = MinFork x []
{-# INLINE _Hsingleton #-}

_Hnull :: MinHeap a -> Bool
_Hnull MinEmpty      = True
_Hnull (MinFork _ _) = False
{-# INLINE _Hnull #-}

_Hinsert :: Ord a => a -> MinHeap a -> MinHeap a
_Hinsert x = _Hmerge (MinFork x [])
{-# INLINE _Hinsert #-}

_HminElem :: MinHeap a -> Maybe a
_HminElem (MinFork x _) = Just x
_HminElem MinEmpty      = Nothing
{-# INLINE _HminElem #-}

_HdeleteMin :: Ord a => MinHeap a -> Maybe (MinHeap a)
_HdeleteMin (MinFork _ hs) = Just $ _HmergePairs hs
_HdeleteMin MinEmpty       = Nothing
{-# INLINE _HdeleteMin #-}

_HdeleteFindMin :: Ord a => MinHeap a -> Maybe (a, MinHeap a)
_HdeleteFindMin (MinFork x hs) = Just (x, _HmergePairs hs)
_HdeleteFindMin MinEmpty       = Nothing
{-# INLINE _HdeleteFindMin #-}

_Hmerge :: Ord a => MinHeap a -> MinHeap a -> MinHeap a
_Hmerge hx@(MinFork x hxs) hy@(MinFork y hys)
  | x <= y    = MinFork x (hy:hxs)
  | otherwise = MinFork y (hx:hys)
_Hmerge MinEmpty hy = hy
_Hmerge hx _ = hx
{-# INLINE _Hmerge #-}

_HmergePairs :: Ord a => [MinHeap a] -> MinHeap a
_HmergePairs (x:y:hs) = (x <> y) <> _HmergePairs hs
_HmergePairs [x]      = x
_HmergePairs []       = MinEmpty
{-# INLINE _HmergePairs #-}

instance Ord a => Eq (MinHeap a) where
    (==) = (==) `on` toList

instance Ord a => Ord (MinHeap a) where
    compare = compare `on` toList

instance Ord a => IsList (MinHeap a) where
    type Item (MinHeap a) = a
    fromList xs = _HmergePairs $ map _Hsingleton xs
    toList = L.unfoldr _HdeleteFindMin

instance (Show a, Ord a) => Show (MinHeap a) where
    show = show . toList

instance Ord a => Monoid (MinHeap a) where
    mempty = _Hempty
    {-# INLINE mempty #-}
    mconcat = _HmergePairs
    {-# INLINE mconcat #-}
    mappend = _Hmerge
    {-# INLINE mappend #-}

radixSort64 :: U.Vector Word64 -> U.Vector Word64
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

radixSort
    :: (U.Unbox a, Word64Encode a)
    => U.Vector a -> U.Vector a
radixSort = U.map decode64 . radixSort64 . U.map encode64
{-# INLINE radixSort #-}

radixSortNonNegative
    :: (U.Unbox a, Word64Encode a)
    => U.Vector a -> U.Vector a
radixSortNonNegative
    = U.map decodeNonNegative64
    . radixSort64
    . U.map encodeNonNegative64
{-# INLINE radixSortNonNegative #-}

class Word64Encode a where
    encode64 :: a -> Word64
    decode64 :: Word64 -> a
    -- | for non-negative
    encodeNonNegative64 :: a -> Word64
    encodeNonNegative64 = encode64
    -- | for non-negative
    decodeNonNegative64 :: Word64 -> a
    decodeNonNegative64 = decode64

instance Word64Encode Int where
    encode64 x = unsafeCoerce $ x + 0x3fffffffffffffff
    decode64 x = unsafeCoerce x - 0x3fffffffffffffff
    encodeNonNegative64 = unsafeCoerce
    decodeNonNegative64 = unsafeCoerce

instance Word64Encode (Int, Int) where
    encode64 (x, y) = unsafeCoerce
        $ unsafeShiftL (x + 0x3fffffff) 31 .|. (y + 0x3fffffff)
    decode64 xy = unsafeCoerce (x, y)
      where
        !x = unsafeShiftR xy 31 - 0x3fffffff
        !y = (xy .&. 0x7fffffff) - 0x3fffffff
    encodeNonNegative64 (x, y) = unsafeCoerce $ unsafeShiftL x 31 .|. y
    decodeNonNegative64 xy = unsafeCoerce (x, y)
      where
        !x = unsafeShiftR xy 31
        !y = xy .&. 0x7fffffff

instance Word64Encode (Int, Int, Int) where
    encode64 (x, y, z) = unsafeCoerce
        $ unsafeShiftL (unsafeShiftL (x + 0xfffff) 21 .|. (y + 0xfffff)) 21 .|. (z + 0xfffff)
    decode64 xyz = unsafeCoerce (x, y, z)
      where
        !x = unsafeShiftR xyz 42 - 0xfffff
        !y = (unsafeShiftR xyz 21 .&. 0x1fffff) - 0xfffff
        !z = xyz .&. 0x1fffff - 0xfffff
    encodeNonNegative64 (x, y, z) = unsafeCoerce
        $ unsafeShiftL (unsafeShiftL x 21 .|. y) 21 .|. z
    decodeNonNegative64 xyz = unsafeCoerce (x, y, z)
      where
        !x = unsafeShiftR xyz 42
        !y = unsafeShiftR xyz 21 .&. 0x1fffff
        !z = xyz .&. 0x1fffff

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
