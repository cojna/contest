{-# OPTIONS_GHC -O2 -funbox-strict-fields #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
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
import           Data.List                   hiding (insert)
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
    [n, m] <- map read.words <$> getLine :: IO [Int]
    xs <- U.unfoldrN n parseInt <$> B.getLine
    es <- U.unfoldrN m parseInt2 <$> B.getContents
    putStrLn.maybe"Impossible"show $ solve n m xs es

solve :: Int -> Int -> U.Vector Int -> U.Vector (Int, Int) -> Maybe Int
solve n m xs es
    | 2 * (n - 1 - m) == 0 = Just 0
    | 2 * (n - 1 - m) == numG + length rest = Just $ sum hs + sum rest
    | otherwise = Nothing
  where
    vss = map (sort.map (U.unsafeIndex xs)) $ forest n xs es
    numG = length vss
    hs = map head vss
    rest = take (2 * (n - 1 - m) - numG) . sort $ concatMap tail vss

forest :: Int -> U.Vector Int -> U.Vector (Int, Int) -> [[Int]]
forest n xs es = runST $ do
    uf <- newUnionFind n
    U.forM_ es $ \(x, y) ->
      uniteM x y uf
    groupsM uf

-------------------------------------------------------------------------------
rep, rev :: Monad m => Int -> (Int -> m ()) -> m ()
rep !n = U.forM_ $ U.generate n id
rev !n = U.forM_ $ U.iterateN n (subtract 1) (n - 1)
for :: Monad m => Int -> Int -> (Int -> m ()) -> m ()
for !s !t = U.forM_ $ U.generate (t - s) (+s)
{-# INLINE rep #-}
{-# INLINE rev #-}
{-# INLINE for #-}

type Parser a = B.ByteString -> Maybe (a, B.ByteString)

parseInt :: Parser Int
parseInt = B.readInt . B.dropWhile isSpace

parseInt2 :: Parser (Int, Int)
parseInt2 = runStateT $
    (,) <$> StateT (B.readInt . B.dropWhile isSpace)
        <*> StateT (B.readInt . B.unsafeTail)


data UnionFind m = UF
    { parent :: UM.MVector m Int
    , rank   :: UM.MVector m Int
    }

nothing :: Int
nothing = -1
{-# INLINE nothing #-}

newUnionFind :: PrimMonad m => Int -> m (UnionFind (PrimState m))
newUnionFind n = UF `liftM` UM.replicate n nothing `ap` UM.replicate n 0
{-# INLINE newUnionFind #-}

findM :: PrimMonad m => Int -> UnionFind (PrimState m) -> m Int
findM x uf@UF{..} = do
    px <- UM.unsafeRead parent x
    if px == nothing
    then return x
    else do
        ppx <- findM px uf
        UM.unsafeWrite parent x ppx
        return ppx
{-# INLINE findM #-}

uniteM :: PrimMonad m => Int -> Int -> UnionFind (PrimState m) -> m ()
uniteM x y uf@UF{..} = do
    px <- findM x uf
    py <- findM y uf
    when (px /= py) $ do
        rx <- UM.unsafeRead rank px
        ry <- UM.unsafeRead rank py
        case compare rx ry of
            LT -> UM.unsafeWrite parent px py
            GT -> UM.unsafeWrite parent py px
            EQ -> do
                UM.unsafeWrite parent px py
                UM.unsafeWrite rank py (ry + 1)
{-# INLINE uniteM #-}

equivM :: PrimMonad m => Int -> Int -> UnionFind (PrimState m) -> m Bool
equivM x y uf = (==) `liftM` findM x uf `ap` findM y uf
{-# INLINE equivM #-}

-- | O(n)
countM :: PrimMonad m => UnionFind (PrimState m) -> m Int
countM UF{..} = liftM (U.length . U.filter (==nothing)) $ U.unsafeFreeze parent
{-# INLINE countM #-}

groupsM :: PrimMonad m => UnionFind (PrimState m) -> m [[Int]]
groupsM uf@UF{..} = do
    gs <- U.foldM (\m i-> do
       p <- findM i uf
       return $ IM.insertWith (++) p [i] m
       ) IM.empty (U.generate (UM.length parent) id)
    return . map snd $ IM.toList gs
