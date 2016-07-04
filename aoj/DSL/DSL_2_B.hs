{-# OPTIONS_GHC -O2 -funbox-strict-fields #-}
{-# LANGUAGE BangPatterns #-}

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Data.Bits
import qualified Data.ByteString.Char8       as B
import qualified Data.ByteString.Unsafe      as B
import           Data.Char
import           Data.Monoid
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM

main :: IO ()
main = do
    [n, q] <- map read.words <$> getLine :: IO [Int]
    queries <- U.unfoldrN q (readInt3.B.dropWhile isSpace) <$> B.getContents
    putStr.unlines.map show $ solve n queries

solve :: Int -> U.Vector (Int, Int, Int) -> [Int]
solve n queries = ($[]) $ runST $ do
    bit <- newBinaryIndexedTree n
    U.foldM' `flip` id `flip` queries $ \acc (com, x, y) ->
        if com == 0
        then updateKey (x-1) y bit >> return acc
        else (acc.).(:) <$> queryRange (x-1) (y-1) bit

instance Monoid Int where
    mempty = 0
    mappend = (+)
    {-# INLINE mempty #-}
    {-# INLINE mappend #-}

instance Group Int where
    invert = negate
    {-# INLINE invert #-}

-------------------------------------------------------------------------------

readInt3 :: B.ByteString -> Maybe ((Int,Int,Int), B.ByteString)
readInt3 bs = Just ((x,y,z),bsz)
  where
    Just (x, bsx) = B.readInt bs
    Just (y, bsy) = B.readInt $ B.unsafeTail bsx
    Just (z, bsz) = B.readInt $ B.unsafeTail bsy

class Monoid g => Group g where
    invert :: g -> g

newtype BinaryIndexedTree m g = BIT (UM.MVector m g)

newBinaryIndexedTree :: (PrimMonad m, U.Unbox g, Group g)
                     => Int
                     -> m (BinaryIndexedTree (PrimState m) g)
newBinaryIndexedTree n = BIT `liftM` UM.replicate n mempty

queryKey :: (PrimMonad m, U.Unbox g, Group g)
         => Int
         -> BinaryIndexedTree (PrimState m) g
         -> m g
queryKey key (BIT bit) = go key mempty
  where
    go !i !res
      | i >= 0 = UM.unsafeRead bit i >>= go ((i .&. (i + 1)) - 1) . (<> res)
      | otherwise = return res
{-# INLINE queryKey #-}

queryRange :: (PrimMonad m, U.Unbox g, Group g)
           => Int
           -> Int
           -> BinaryIndexedTree (PrimState m) g
           -> m g
queryRange 0 r bit = queryKey r bit
queryRange l r bit = (mappend.invert) `liftM` queryKey (l - 1) bit `ap` queryKey r bit
{-# INLINE queryRange #-}

updateKey :: (PrimMonad m, U.Unbox g, Group g)
          => Int
          -> g
          -> BinaryIndexedTree (PrimState m) g
          -> m ()
updateKey key !x (BIT bit) = go key
  where
    !n = UM.length bit
    go !i = when (i < n) $ do
      UM.unsafeRead bit i >>= UM.unsafeWrite bit i . (<> x)
      go $ i .|. (i + 1)
{-# INLINE updateKey #-}

