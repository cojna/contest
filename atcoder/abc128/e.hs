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
    [n, q] <- map read.words <$> getLine :: IO [Int]
    stxd <- U.unfoldrN (3 * n + q) (C.readInt.C.dropWhile isSpace) <$> C.getContents
    B.hPutBuilder IO.stdout
        . U.foldr (\x acc -> B.intDec x <> B.char7 '\n' <> acc) mempty
        $ solve n q (U.take (3 * n) stxd) (U.drop (3 * n) stxd)

solve :: Int -> Int -> U.Vector Int -> U.Vector Int -> U.Vector Int
solve n q stxs ds = U.map (convert . U.unsafeIndex segtree . (ks IM.!)) ds
  where
    (ss, ts, xs) = U.unzip3 . flip (U.unfoldrN n) stxs $ \vec ->
        Just ((U.unsafeIndex vec 0, U.unsafeIndex vec 1, U.unsafeIndex vec 2), U.drop 3 vec)

    offset :: Int
    offset = 1000000000

    ks :: IM.IntMap Int
    !ks = U.foldl' (\m (i, x) -> IM.insert x i m) IM.empty $ U.indexed vs

    vs :: U.Vector Int
    vs = U.map (subtract offset)
        . unsafeCoerce
        . radixSort64
        . unsafeCoerce
        . U.map (+offset)
        $ U.concat [ds, U.zipWith (-) ss xs, U.zipWith (-) ts xs]

    segtree :: U.Vector Int
    !segtree = runST $ do
        seg <- newSegTree (2 * n + q) maxBound
        U.forM_ (U.zip3 ss ts xs) $ \(s,t,x) -> do
            let l = ks IM.! (s - x)
            let r = ks IM.! (t - x)
            updateSegTree l r x seg
        freezeSegTree seg

    convert :: Int -> Int
    convert x
        | x == maxBound = -1
        | otherwise = x


newtype SegTree m a = SegTree { unSegTree :: UM.MVector m a }

newSegTree :: (Monoid a, U.Unbox a, PrimMonad m)
    => Int -> a -> m (SegTree (PrimState m) a)
newSegTree n x = SegTree <$> UM.replicate (2 * extendToPowerOfTwo n) x

updateSegTree :: (Show a, Monoid a, U.Unbox a, PrimMonad m)
    => Int -> Int -> a -> SegTree (PrimState m) a -> m ()
updateSegTree l r x segtree = do
    let tree = unSegTree segtree
    let n = UM.length tree .>>. 1
    let go !l !r = when (l < r) $ do
            when (l .&. 1 == 1) $ do
                UM.unsafeModify tree (mappend x) l
            when (r .&. 1 == 1) $ do
                UM.unsafeModify tree (mappend x) (r - 1)
            go ((l + l .&. 1) .>>. 1) ((r - r .&. 1) .>>. 1)
    go (n + l) (n + r)

freezeSegTree :: (Show a, Monoid a, U.Unbox a, PrimMonad m)
    => SegTree (PrimState m) a -> m (U.Vector a)
freezeSegTree segtree = do
    let tree = unSegTree segtree
    let n = UM.length tree .>>. 1
    U.forM_ (U.generate (n - 1) (+1)) $ \i -> do
        x <- UM.unsafeRead tree i
        UM.unsafeModify tree (mappend x) $ i .<<. 1
        UM.unsafeModify tree (mappend x) $ (i .<<. 1) .|. 1
    U.unsafeFreeze $ UM.unsafeSlice n n tree

instance Monoid Int where
    mempty = maxBound
    {-# INLINE mempty #-}
    mappend = min
    {-# INLINE mappend #-}

-------------------------------------------------------------------------------
extendToPowerOfTwo :: Int -> Int
extendToPowerOfTwo x
    | w > 1 = fromIntegral
        $ unsafeShiftR (maxBound :: Word) (countLeadingZeros (w - 1)) + 1
    | otherwise = 1
  where
    w :: Word
    w = fromIntegral x

infixl 8 .<<., .>>.
(.<<.) :: Int -> Int -> Int
(.<<.) = unsafeShiftL
{-# INLINE (.<<.) #-}
(.>>.) :: Int -> Int -> Int
(.>>.) = unsafeShiftR
{-# INLINE (.>>.) #-}

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