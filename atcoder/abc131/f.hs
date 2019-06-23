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
    n <- readLn
    xys <- U.unfoldrN n parseInt2 <$> C.hGetNonBlocking IO.stdin (14 * n)
    print $ solve n xys

lim :: Int
lim = 100001

injX :: Int -> Int
injX x = x

injY :: Int -> Int
injY y = y + lim

solve :: Int -> U.Vector (Int, Int) -> Int
solve n xys = subtract n . U.sum . U.map step $ U.filter (<0) rank
  where
    rank = runST $ do
        dat <- UM.replicate (2 * lim) (negate $ bit 32)
        UM.set (UM.take lim dat) $ negate $ bit 32 + 1
        let uf = UF dat
        U.forM_ xys $ \(x, y) ->
            uniteM uf x $ injY y
        U.forM_ (U.generate (2 * lim) id) $ \i -> do
            findM uf i
        U.unsafeFreeze $ parent uf
    step x = case negate x of
        y -> (y .&. 0xffffffff) * (unsafeShiftR y 32 - (y .&. 0xffffffff))

-------------------------------------------------------------------------------
newtype UnionFind m = UF { parent :: UM.MVector m Int }

newUnionFind :: PrimMonad m => Int -> m (UnionFind (PrimState m))
newUnionFind n = UF <$> UM.replicate n (-1)
{-# INLINE newUnionFind #-}

findM :: PrimMonad m => UnionFind (PrimState m) -> Int -> m Int
findM uf x = go x return
  where
    go !x k = do
        px <- UM.unsafeRead (parent uf) x
        if px < 0
        then k x
        else go px $ \ppx -> do
            UM.unsafeWrite (parent uf) x ppx
            k ppx
{-# INLINE findM #-}

sizeM :: PrimMonad m => UnionFind (PrimState m) -> Int -> m Int
sizeM uf = fix $ \loop x -> do
    px <- UM.unsafeRead (parent uf) x
    if px < 0
    then return $! negate px
    else loop px
{-# INLINE sizeM #-}

uniteM :: PrimMonad m => UnionFind (PrimState m) -> Int -> Int -> m Bool
uniteM uf x y = do
    px <- findM uf x
    py <- findM uf y
    if px == py
    then return False
    else do
        rx <- UM.unsafeRead (parent uf) px
        ry <- UM.unsafeRead (parent uf) py
        if rx < ry
        then do
            UM.unsafeModify (parent uf) (+ry) px
            UM.unsafeWrite (parent uf) py px
        else do
            UM.unsafeModify (parent uf) (+rx) py
            UM.unsafeWrite (parent uf) px py
        return True
{-# INLINE uniteM #-}

equivM :: PrimMonad m => UnionFind (PrimState m) -> Int -> Int -> m Bool
equivM uf x y = (==) `liftM` findM uf x `ap` findM uf y
{-# INLINE equivM #-}

-- | O(n)
countGroupM :: PrimMonad m => UnionFind (PrimState m) -> m Int
countGroupM uf = U.length . U.filter (<0) <$> U.unsafeFreeze (parent uf)
{-# INLINE countGroupM #-}

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
