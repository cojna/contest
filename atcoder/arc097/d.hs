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
import qualified Data.Set                         as S
import           Data.STRef
import qualified Data.Traversable                 as T
import           Data.Tuple
import           Data.Word
import           Debug.Trace
import           GHC.Arr                          (Array, Ix (..), STArray)
import           GHC.Exts
import           System.Exit
import           System.IO

main :: IO ()
main = do
    [n, m] <- map read.words <$> getLine
    ps <- map (subtract 1) . L.unfoldr (runStateT parseInt) <$> B.getLine
    xys <- map (\(x,y) -> (x-1, y-1)) . L.unfoldr (runStateT parseInt2) <$> B.getContents
    print $ solve n ps xys

solve :: Int -> [Int] -> [(Int, Int)] -> Int
solve n ps xys = runST $ do
    uf <- newUnionFindST n
    F.forM_ xys $ \(x,y) ->
      uniteM uf x y

    F.foldlM (\ !res (i, p) -> do
        equiv <- equivM uf i p
        if equiv
        then return $! res + 1
        else return $! res
        ) 0 $ zip [0..] ps

-------------------------------------------------------------------------------
nothing :: Int
nothing = -1
{-# INLINE nothing #-}

type Parent = Int
type Rank = Int
data UnionFindST s = UF
    { size   :: !Int
    , parent :: !(STUArray s Int Parent)
    , rank   :: !(STUArray s Int Rank)
    }

newUnionFindST :: Int -> ST s (UnionFindST s)
newUnionFindST n = UF n
    <$> newArray (0, n - 1) nothing
    <*> newArray (0, n - 1) 0

findM :: UnionFindST s -> Int -> ST s Parent
findM uf@UF{..} x = do
    px <- unsafeRead parent x
    if px == nothing
    then return x
    else do
        ppx <- findM uf px
        unsafeWrite parent x ppx
        return ppx

uniteM :: UnionFindST s -> Int -> Int -> ST s ()
uniteM uf@UF{..} x y = do
    px <- findM uf x
    py <- findM uf y
    when (px /= py) $ do
        rx <- unsafeRead rank px
        ry <- unsafeRead rank py
        case compare rx ry of
            LT -> unsafeWrite parent px py
            GT -> unsafeWrite parent py px
            EQ -> do
                unsafeWrite parent px py
                unsafeWrite rank py $ ry + 1

equivM :: UnionFindST s -> Int -> Int -> ST s Bool
equivM uf x y = (==) <$> findM uf x <*> findM uf y

countGroupM :: UnionFindST s -> ST s Int
countGroupM UF{..} = fix `flip` 0 `flip` 0 $ \loop !i !res ->
    if i < size
    then do
        p <- unsafeRead parent i
        if p == nothing
        then loop (i + 1) (res + 1)
        else loop (i + 1) res
    else return res

type Parser a = StateT B.ByteString Maybe a

parseInt :: Parser Int
parseInt = coerce $ B.readInt . B.dropWhile isSpace

parseInt2 :: Parser (Int, Int)
parseInt2 = (,) <$> parseInt <*> parseInt

parseInt3 :: Parser (Int, Int, Int)
parseInt3 = (,,) <$> parseInt <*> parseInt <*> parseInt

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

