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
    n <- readLn :: IO Int
    xs <- U.unfoldrN (2^n) parseInt <$> C.getLine
    putStrLn.bool"No""Yes" $ solve n xs

solve :: Int -> U.Vector Int -> Bool
solve n xs = maybe False ((== U.length xs) . U.length) $ evalStateT (simulate $ U.singleton mm) set0
  where
    Just (mm, set0) = maxViewIMS $ U.foldl' (flip insertIMS) emptyIMS xs

    spawn :: Int -> StateT IntMultiSet Maybe Int
    spawn x = do
        ims <- get
        case lookupLTIMS x ims of
            Just x' -> do
                modify' $ deleteIMS x'
                return x'
            Nothing -> mzero


    simulate :: U.Vector Int -> StateT IntMultiSet Maybe (U.Vector Int)
    simulate !xs = do
        ms <- get
        if nullIMS ms
        then return xs
        else do
            xs' <- U.mapM spawn xs
            simulate $ xs <> xs'

-------------------------------------------------------------------------------
newtype IntMultiSet = IntMultiSet { getIntMultiSet :: IM.IntMap Int }
    deriving (Eq)

instance Show IntMultiSet where
    show = show . toList

instance IsList IntMultiSet where
    type Item IntMultiSet = Int
    fromList = L.foldl' (flip insertIMS) emptyIMS
    toList = concatMap (\(k, x) -> replicate x k)
        . IM.toList
        . (coerce :: IntMultiSet -> IM.IntMap Int)

emptyIMS :: IntMultiSet
emptyIMS = coerce (IM.empty :: IM.IntMap Int)

singletonIMS :: Int -> IntMultiSet
singletonIMS x = coerce $ IM.singleton x (1 :: Int)

-- | /O(min(n,W))/
insertIMS :: Int -> IntMultiSet -> IntMultiSet
insertIMS x = coerce . IM.insertWith (+) x (1 :: Int) . coerce

-- | /O(min(n,W))/
deleteIMS :: Int -> IntMultiSet -> IntMultiSet
deleteIMS x = (coerce :: IM.IntMap Int -> IntMultiSet)
    . IM.update (\y -> if y > 1 then Just $! y - 1 else Nothing) x
    . coerce

-- | /O(min(n,W))/
deleteAllIMS :: Int -> IntMultiSet -> IntMultiSet
deleteAllIMS x = (coerce :: IM.IntMap Int -> IntMultiSet)
    . IM.delete x
    . coerce

-- | /O(min(n,W))/
memberIMS :: Int -> IntMultiSet -> Bool
memberIMS x = IM.member x . (coerce :: IntMultiSet -> IM.IntMap Int)

-- | /O(min(n,W))/
notMemberIMS :: Int -> IntMultiSet -> Bool
notMemberIMS x = not . memberIMS x

-- | /O(log n)/
lookupLTIMS :: Int -> IntMultiSet -> Maybe Int
lookupLTIMS x = fmap fst . IM.lookupLT x
    . (coerce :: IntMultiSet -> IM.IntMap Int)

-- | /O(log n)/
lookupGTIMS :: Int -> IntMultiSet -> Maybe Int
lookupGTIMS x = fmap fst . IM.lookupGT x
    . (coerce :: IntMultiSet -> IM.IntMap Int)

-- | /O(log n)/
lookupLEIMS :: Int -> IntMultiSet -> Maybe Int
lookupLEIMS x = fmap fst . IM.lookupLE x
    . (coerce :: IntMultiSet -> IM.IntMap Int)

-- | /O(log n)/
lookupGEIMS :: Int -> IntMultiSet -> Maybe Int
lookupGEIMS x = fmap fst . IM.lookupGE x
    . (coerce :: IntMultiSet -> IM.IntMap Int)

-- | /O(1)/
nullIMS :: IntMultiSet -> Bool
nullIMS = IM.null
    . (coerce :: IntMultiSet -> IM.IntMap Int)

-- | /O(n)/
sizeIMS :: IntMultiSet -> Int
sizeIMS = IM.foldl' (+) 0
    . (coerce :: IntMultiSet -> IM.IntMap Int)

-- | /O(min(n,W))/
findMinIMS :: IntMultiSet -> Int
findMinIMS = fst
    . IM.findMin
    . (coerce :: IntMultiSet -> IM.IntMap Int)
-- | /O(min(n,W))/
findMaxIMS :: IntMultiSet -> Int
findMaxIMS = fst
    . IM.findMax
    . (coerce :: IntMultiSet -> IM.IntMap Int)

-- | /O(min(n,W))/
deleteMinIMS :: IntMultiSet -> IntMultiSet
deleteMinIMS = coerce
    . IM.updateMin (\x -> if x > 1 then Just $! x - 1 else Nothing)
    . (coerce :: IntMultiSet -> IM.IntMap Int)

-- | /O(min(n,W))/
deleteMaxIMS :: IntMultiSet -> IntMultiSet
deleteMaxIMS = coerce
    . IM.updateMax (\x -> if x > 1 then Just $! x - 1 else Nothing)
    . (coerce :: IntMultiSet -> IM.IntMap Int)

-- | /O(min(n,W))/
maxViewIMS :: IntMultiSet -> Maybe (Int, IntMultiSet)
maxViewIMS = maybe Nothing just
    . IM.maxViewWithKey
    . (coerce :: IntMultiSet -> IM.IntMap Int)
  where
    just :: ((Int, Int), IM.IntMap Int) -> Maybe (Int, IntMultiSet)
    just ((k, x), m)
        | x > 1 = case IM.insert k (x - 1) m of
            m' -> Just (k, coerce m')
        | otherwise = Just (k, coerce m)

-- | /O(min(n,W))/
minViewIMS :: IntMultiSet -> Maybe (Int, IntMultiSet)
minViewIMS = maybe Nothing just
    . IM.minViewWithKey
    . (coerce :: IntMultiSet -> IM.IntMap Int)
  where
    just :: ((Int, Int), IM.IntMap Int) -> Maybe (Int, IntMultiSet)
    just ((k, x), m)
        | x > 1 = case IM.insert k (x - 1) m of
            m' -> Just (k, coerce m')
        | otherwise = Just (k, coerce m)

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