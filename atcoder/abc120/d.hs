{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Control.Monad.State.Strict
import           Data.Bool
import qualified Data.ByteString.Char8       as B
import qualified Data.ByteString.Unsafe      as B
import           Data.Char
import qualified Data.Foldable               as F
import           Data.Function
import qualified Data.IntMap.Strict          as IM
import qualified Data.IntSet                 as IS
import           Data.List
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
import           Unsafe.Coerce

main :: IO ()
main = do
    [n, m] <- map read.words <$> getLine :: IO [Int]
    xys <- U.unfoldrN m parseInt2 <$> B.getContents
    putStr.unlines.map show $ solve n m xys

solve :: Int -> Int -> U.Vector (Int, Int) -> [Int]
solve n m xys = runST $ do
    uf <- newUnionFind n
    let go !acc !res ((x, y):rest) = do
            sx <- sizeM uf $ x - 1
            sy <- sizeM uf $ y - 1
            united <- uniteM uf (x - 1) (y - 1)
            if united
            then go (acc - sx * sy) (acc:res) rest
            else go acc (acc:res) rest
        go _ res [] = return res
    go (n * (n - 1) `div` 2) [] . U.toList $ U.reverse xys

-------------------------------------------------------------------------------
type Parser a = B.ByteString -> Maybe (a, B.ByteString)

parseInt :: Parser Int
parseInt = B.readInt . B.dropWhile isSpace

parseInt2 :: Parser (Int, Int)
parseInt2 = runStateT $
    (,) <$> StateT (B.readInt . B.dropWhile isSpace)
        <*> StateT (B.readInt . B.unsafeTail)

parseInt3 :: Parser (Int, Int, Int)
parseInt3 = runStateT $
    (,,) <$> StateT (B.readInt . B.dropWhile isSpace)
        <*> StateT (B.readInt . B.unsafeTail)
        <*> StateT (B.readInt . B.unsafeTail)

parseInt4 :: Parser (Int, Int, Int, Int)
parseInt4 = runStateT $
    (,,,) <$> StateT (B.readInt . B.dropWhile isSpace)
        <*> StateT (B.readInt . B.unsafeTail)
        <*> StateT (B.readInt . B.unsafeTail)
        <*> StateT (B.readInt . B.unsafeTail)

newtype UnionFind m = UF { parent :: UM.MVector m Int }

newUnionFind :: PrimMonad m => Int -> m (UnionFind (PrimState m))
newUnionFind n = UF <$> UM.replicate n (-1)
{-# INLINE newUnionFind #-}

findM :: PrimMonad m => UnionFind (PrimState m) -> Int -> m Int
findM uf@UF{..} x = do
    px <- UM.unsafeRead parent x
    if px < 0
    then return x
    else do
        ppx <- findM uf px
        UM.unsafeWrite parent x ppx
        return ppx
{-# INLINE findM #-}

sizeM :: PrimMonad m => UnionFind (PrimState m) -> Int -> m Int
sizeM uf@UF{..} = fix $ \loop x -> do
    px <- UM.unsafeRead parent x
    if px < 0
    then return $! negate px
    else loop px
{-# INLINE sizeM #-}

uniteM :: PrimMonad m => UnionFind (PrimState m) -> Int -> Int -> m Bool
uniteM uf@UF{..} x y = do
    px <- findM uf x
    py <- findM uf y
    if px == py
    then return False
    else do
        rx <- UM.unsafeRead parent px
        ry <- UM.unsafeRead parent py
        if rx < ry
        then do
            UM.unsafeModify parent (+ry) px
            UM.unsafeWrite parent py px
        else do
            UM.unsafeModify parent (+rx) py
            UM.unsafeWrite parent px py
        return True
{-# INLINE uniteM #-}

equivM :: PrimMonad m => UnionFind (PrimState m) -> Int -> Int -> m Bool
equivM uf x y = (==) `liftM` findM uf x `ap` findM uf y
{-# INLINE equivM #-}

-- | O(n)
countGroupM :: PrimMonad m => UnionFind (PrimState m) -> m Int
countGroupM UF{..} = U.length . U.filter (<0) <$> U.unsafeFreeze parent
{-# INLINE countGroupM #-}