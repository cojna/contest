{-# LANGUAGE BangPatterns, CPP #-}

import           Control.Monad
import qualified Data.List      as L

main :: IO ()
main = runGCJ $ do
    n <- readLn
    mat <- replicateM n $ do
        map read.words <$> getLine
    putStrLn.unwords.map show $ solve n mat

solve :: Int -> [[Int]] -> [Int]
solve n mat = [k, r, c]
  where
    !k = sum . map head $ zipWith drop [0..] mat
    !r = length $ filter ((/= [1..n]).L.sort) mat
    !c = length . filter ((/= [1..n]).L.sort) $ L.transpose mat

runGCJ :: IO () -> IO ()
#ifdef DEBUG
runGCJ = id
#else
runGCJ action = do
    t <- readLn
    forM_ [1..t] $ \i -> do
        putStr $ "Case #" ++ shows i ": "
        action
#endif
