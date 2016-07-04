{-# OPTIONS_GHC -O2 -funbox-strict-fields #-}
{-# LANGUAGE BangPatterns #-}

import           Control.Applicative
import qualified Data.ByteString.Char8  as B
import qualified Data.ByteString.Unsafe as B
import           Data.Char
import           Data.Monoid
import qualified Data.Vector.Unboxed    as U

main :: IO ()
main = do
    [n, q] <- map read.words <$> getLine :: IO [Int]
    queries <- U.unfoldrN q (readInt3.B.dropWhile isSpace) <$> B.getContents
    putStr.unlines.map show $ solve n queries

solve :: Int -> U.Vector (Int, Int, Int) -> [Int]
solve n queries = ($[]) . fst $ U.foldl' step  (id, segtree0) queries
  where
    segtree0 = toSegTree $ U.replicate n mempty
    step (!acc, !segtree) (com, x, y)
        | com == 0 = (acc, updateKey x (const y) segtree)
        | otherwise = let !v = queryRange x y segtree
                      in (acc.(v:), segtree)

instance Monoid Int where
    mempty = 0x7fffffff
    mappend = min
    {-# INLINE mempty #-}
    {-# INLINE mappend #-}

-------------------------------------------------------------------------------

readInt3 :: B.ByteString -> Maybe ((Int,Int,Int), B.ByteString)
readInt3 bs = Just ((x,y,z),bsz)
  where
    Just (x, bsx) = B.readInt bs
    Just (y, bsy) = B.readInt $ B.unsafeTail bsx
    Just (z, bsz) = B.readInt $ B.unsafeTail bsy

data SegTree m
    = Node
    {-# UNPACK #-} !Int
    {-# UNPACK #-} !Int
    !m
    !(SegTree m)
    !(SegTree m)
    | Leaf
    {-# UNPACK #-} !Int
    !m
    deriving Show


extract :: SegTree m -> m
extract (Node _ _ m _ _) = m
extract (Leaf _ m) = m
{-# INLINE extract #-}

toSegTree :: (U.Unbox m, Monoid m) => U.Vector m -> SegTree m
toSegTree vec = go 0 (U.length vec - 1)
  where
    go !l !r
        | l < r = let !m = (l + r) `quot` 2
                      lt = go l m
                      rt = go (m + 1) r
                  in Node l r (extract lt <> extract rt) lt rt
        | otherwise = Leaf l $ U.unsafeIndex vec l

updateKey :: Monoid m => Int -> (m -> m) -> SegTree m -> SegTree m
updateKey key f = go
  where
    go segtree@(Node l r _ lt rt)
        | key < l || r < key = segtree
        | 2 * key <= l + r = let !lt' = go lt
                             in Node l r (extract lt' <> extract rt) lt' rt
        | otherwise = let !rt' = go rt
                      in Node l r (extract lt <> extract rt') lt rt'
    go segtree@(Leaf k m)
        | k == key = Leaf k (f m)
        | otherwise = segtree
{-# INLINE updateKey #-}

queryKey :: Monoid m => Int -> SegTree m -> m
queryKey key = go
  where
    go (Node l r _ lt rt)
        | key < l || r < key = mempty
        | 2 * key <= l + r = go lt
        | otherwise = go rt
    go (Leaf k m)
        | k == key = m
        | otherwise = mempty
{-# INLINE queryKey #-}

queryRange :: Monoid m => Int -> Int -> SegTree m -> m
queryRange kl kr = go
  where
    go (Node l r m lt rt)
        | r < kl || kr < l = mempty
        | kl <= l && r <= kr = m
        | otherwise = go lt <> go rt
    go (Leaf k m)
        | kl <= k && k <= kr = m
        | otherwise = mempty
{-# INLINE queryRange #-}
