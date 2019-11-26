{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns, CPP, FlexibleContexts, FlexibleInstances         #-}
{-# LANGUAGE KindSignatures, LambdaCase, MagicHash, MultiParamTypeClasses   #-}
{-# LANGUAGE MultiWayIf, OverloadedStrings, RecordWildCards                 #-}
{-# LANGUAGE ScopedTypeVariables, TupleSections, TypeFamilies, ViewPatterns #-}

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
import           Data.Functor.Identity
import qualified Data.IntMap.Strict          as IM
import qualified Data.IntSet                 as IS
import qualified Data.List                   as L
import qualified Data.Map.Strict             as M
import           Data.Monoid
import           Data.Ord
import           Data.Primitive.MutVar
import           Data.Ratio
import qualified Data.Set                    as S
import           Data.Tuple
import qualified Data.Vector                 as V
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Mutable         as VM
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM
import           Debug.Trace
import           Foreign                     hiding (void)
import           GHC.Exts
import qualified System.IO                   as IO
import           Unsafe.Coerce

main :: IO ()
main = do
    [n, k] <- map read.words <$> getLine :: IO [Int]
    xs <- U.unfoldrN n (runParser int) <$> C.getLine
    print $ solve n k xs

solve :: Int -> Int -> U.Vector Int -> Int
solve n k xs = fst . U.foldl' step (res0, ims0) . U.zip psum $ U.drop k psum
  where
    res0 = IM.foldl' (\acc l -> acc + l * (l - 1) `quot` 2) 0 $ getIntMultiSet ims0
    ims0 = U.foldl' (flip insertIMS) emptyIMS $ U.take k psum
    step (!res, !ims) (front, x) = (res', inserted)
      where
        !res' = res + countIMS x deleted
        !deleted = deleteIMS front ims
        !inserted = insertIMS x deleted
    psum = U.map (flip rem k)
        . U.scanl' (+) 0
        $ U.map (subtract 1) xs

countIMS :: Int -> IntMultiSet -> Int
countIMS key = IM.findWithDefault 0 key . coerce
{-# INLINE countIMS #-}

-------------------------------------------------------------------------------
-- Utils
-------------------------------------------------------------------------------
rep :: Monad m => Int -> (Int -> m ()) -> m ()
rep !n = U.forM_ $ U.generate n id
{-# INLINE rep #-}
rev :: Monad m => Int -> (Int -> m ()) -> m ()
rev !n = U.forM_ $ U.iterateN n (subtract 1) (n - 1)
{-# INLINE rev #-}
infixl 8 `shiftRL`, `unsafeShiftRL`
shiftRL = unsafeShiftRL
{-# INLINE shiftRL #-}
unsafeShiftRL (I# x#) (I# i#) = I# (uncheckedIShiftRL# x# i#)
{-# INLINE unsafeShiftRL #-}
type Parser a = StateT C.ByteString Maybe a
runParser :: Parser a -> C.ByteString -> Maybe (a, C.ByteString)
runParser = runStateT
{-# INLINE runParser #-}
int :: Parser Int
int = coerce $ C.readInt . C.dropWhile isSpace
{-# INLINE int #-}
int1 :: Parser Int
int1 = fmap (subtract 1) int
{-# INLINE int1 #-}
char :: Parser Char
char = coerce C.uncons
{-# INLINE char #-}
byte :: Parser Word8
byte = coerce B.uncons
{-# INLINE byte #-}
-------------------------------------------------------------------------------
-- Data.IntMultiSet
-------------------------------------------------------------------------------
newtype IntMultiSet = IntMultiSet{getIntMultiSet :: IM.IntMap Int} deriving (Eq)
instance Show IntMultiSet where { show = show . toList}
instance IsList IntMultiSet where { type Item IntMultiSet = Int; fromList = L.foldl' (flip insertIMS) emptyIMS; toList = concatMap (\ (k, x) -> replicate x k) . IM.toList . (coerce :: IntMultiSet -> IM.IntMap Int)}
emptyIMS :: IntMultiSet
emptyIMS = coerce (IM.empty :: IM.IntMap Int)
singletonIMS :: Int -> IntMultiSet
singletonIMS x = coerce $ IM.singleton x (1 :: Int)
insertIMS :: Int -> IntMultiSet -> IntMultiSet
insertIMS x = coerce . IM.insertWith (+) x (1 :: Int) . coerce
deleteIMS :: Int -> IntMultiSet -> IntMultiSet
deleteIMS x = (coerce :: IM.IntMap Int -> IntMultiSet) . IM.update (\ y -> if y > 1 then Just $! y - 1 else Nothing) x . coerce
deleteAllIMS :: Int -> IntMultiSet -> IntMultiSet
deleteAllIMS x = (coerce :: IM.IntMap Int -> IntMultiSet) . IM.delete x . coerce
memberIMS :: Int -> IntMultiSet -> Bool
memberIMS x = IM.member x . (coerce :: IntMultiSet -> IM.IntMap Int)
notMemberIMS :: Int -> IntMultiSet -> Bool
notMemberIMS x = not . memberIMS x
lookupLTIMS :: Int -> IntMultiSet -> Maybe Int
lookupLTIMS x = fmap fst . IM.lookupLT x . (coerce :: IntMultiSet -> IM.IntMap Int)
lookupGTIMS :: Int -> IntMultiSet -> Maybe Int
lookupGTIMS x = fmap fst . IM.lookupGT x . (coerce :: IntMultiSet -> IM.IntMap Int)
lookupLEIMS :: Int -> IntMultiSet -> Maybe Int
lookupLEIMS x = fmap fst . IM.lookupLE x . (coerce :: IntMultiSet -> IM.IntMap Int)
lookupGEIMS :: Int -> IntMultiSet -> Maybe Int
lookupGEIMS x = fmap fst . IM.lookupGE x . (coerce :: IntMultiSet -> IM.IntMap Int)
nullIMS :: IntMultiSet -> Bool
nullIMS = IM.null . (coerce :: IntMultiSet -> IM.IntMap Int)
sizeIMS :: IntMultiSet -> Int
sizeIMS = IM.foldl' (+) 0 . (coerce :: IntMultiSet -> IM.IntMap Int)
findMinIMS :: IntMultiSet -> Int
findMinIMS = fst . IM.findMin . (coerce :: IntMultiSet -> IM.IntMap Int)
findMaxIMS :: IntMultiSet -> Int
findMaxIMS = fst . IM.findMax . (coerce :: IntMultiSet -> IM.IntMap Int)
deleteMinIMS :: IntMultiSet -> IntMultiSet
deleteMinIMS = coerce . IM.updateMin (\ x -> if x > 1 then Just $! x - 1 else Nothing) . (coerce :: IntMultiSet -> IM.IntMap Int)
deleteMaxIMS :: IntMultiSet -> IntMultiSet
deleteMaxIMS = coerce . IM.updateMax (\ x -> if x > 1 then Just $! x - 1 else Nothing) . (coerce :: IntMultiSet -> IM.IntMap Int)
maxViewIMS :: IntMultiSet -> Maybe (Int, IntMultiSet)
maxViewIMS = maybe Nothing just . IM.maxViewWithKey . (coerce :: IntMultiSet -> IM.IntMap Int) where { just :: ((Int, Int), IM.IntMap Int) -> Maybe (Int, IntMultiSet); just ((k, x), m) | x > 1 = case IM.insert k (x - 1) m of { m' -> Just (k, coerce m')} | otherwise = Just (k, coerce m)}
minViewIMS :: IntMultiSet -> Maybe (Int, IntMultiSet)
minViewIMS = maybe Nothing just . IM.minViewWithKey . (coerce :: IntMultiSet -> IM.IntMap Int) where { just :: ((Int, Int), IM.IntMap Int) -> Maybe (Int, IntMultiSet); just ((k, x), m) | x > 1 = case IM.insert k (x - 1) m of { m' -> Just (k, coerce m')} | otherwise = Just (k, coerce m)}
