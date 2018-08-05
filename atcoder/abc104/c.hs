{-# LANGUAGE BangPatterns #-}

import           Control.Monad
import           Data.Function
import qualified Data.List     as L
import           Debug.Trace

main :: IO ()
main = do
  [d, g] <- map read.words <$> getLine
  pcs <- replicateM d $ do
    [p, c] <- map read.words <$> getLine
    return (p, c)
  print $ solve d g pcs

solve :: Int -> Int -> [(Int, Int)] -> Int
solve d g pcs = minimum $ do
    complete <- L.subsequences [1..d]
    Just num <- return $ greedy complete
    return num
  where
    !sorted = reverse $ zip [1..d] pcs
    greedy complete = go0 0 0 sorted
      where
        go0 !count !score ((i, (p, c)):rest)
          | i `elem` complete = go0 (count + p) (score + 100 * i * p + c) rest
          | otherwise = go0 count score rest
        go0 count score [] = go count score sorted
        go !count !score ((i, (p, c)):rest)
          | i `elem` complete = go count score rest
          | score < g, q <- min p $ (g - score + 100 * i - 1) `quot` (100 * i) =
              go (count + q) (score + 100 * i * q) rest
          | otherwise = go count score rest
        go count score []
          | score < g = Nothing
          | otherwise = Just count

