{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns #-}

import           Control.Monad
import qualified Data.IntMap.Strict as IM
import           Unsafe.Coerce

main :: IO ()
main = runGCJ $ do
    [n, k] <- map read.words <$> getLine
    let (V2 m mm) = solveSmall n k
    putStrLn.unwords $ map show [mm, m]

data V2 = V2 {-# UNPACK #-} !Int {-# UNPACK #-} !Int deriving (Eq, Ord)

solveSmall :: Int -> Int -> V2
solveSmall n k = go k $ insert n empty
  where
    go !rest heap = case deleteFindMax heap of
        Just (x, h)
          | rest == 1 -> dist x
          | V2 m mm <- dist x -> go (rest - 1)
              . insert m
              $ insert mm h
        Nothing -> V2 0 0

dist :: Int -> V2
dist s
  | r == 0 = V2 (q - 1) q
  | otherwise = V2 q q
  where
    (!q, !r) = quotRem s 2

newtype MultiIntSet = MIS {unMIS :: IM.IntMap Int}

empty :: MultiIntSet
empty =  MIS IM.empty

isEmpty :: MultiIntSet -> Bool
isEmpty = IM.null . unMIS

size :: MultiIntSet -> Int
size = IM.foldl' (+) 0 . unMIS

member :: Int -> MultiIntSet -> Bool
member x = IM.member x . unMIS

lookupLT :: Int -> MultiIntSet -> Maybe Int
lookupLT x = fmap fst . IM.lookupLT x . unMIS

lookupGT :: Int -> MultiIntSet -> Maybe Int
lookupGT x = fmap fst . IM.lookupGT x . unMIS

lookupLE :: Int -> MultiIntSet -> Maybe Int
lookupLE x = fmap fst . IM.lookupLE x . unMIS

lookupGE :: Int -> MultiIntSet -> Maybe Int
lookupGE x = fmap fst . IM.lookupGE x . unMIS

insert :: Int -> MultiIntSet -> MultiIntSet
insert x = MIS . IM.insertWith (+) x 1 . unMIS

delete :: Int -> MultiIntSet ->  MultiIntSet
delete x mset = case IM.lookup x $ unMIS mset of
    Just n | n > 1 -> MIS . IM.insert x (n-1) $ unMIS mset
           | otherwise -> MIS . IM.delete x $ unMIS mset
    Nothing -> mset

deleteFindMax :: MultiIntSet -> Maybe (Int, MultiIntSet)
deleteFindMax mset = case IM.maxViewWithKey $ unMIS mset of
    Just ((m, n), ms)
      | n > 1 -> Just (m, MIS $ IM.insert m (n - 1) ms)
      | otherwise -> Just (m, MIS ms)
    Nothing -> Nothing

runGCJ :: IO () -> IO ()
runGCJ main_ = do
    t <- readLn
    forM_ [1..t] $ \i -> do
        putStr $ "Case #" ++ shows i ": "
        main_
