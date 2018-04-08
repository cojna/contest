{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns #-}

import           Control.Monad
import qualified Data.ByteString.Char8 as B
import           Data.Char
import qualified Data.List             as L
import           Debug.Trace

main :: IO ()
main = runGCJ $ do
    n <- readLn :: IO Int
    xs <- L.unfoldr (B.readInt . B.dropWhile isSpace) <$> B.getLine
    putStrLn . maybe "OK" show $ solve n xs

solve :: Int -> [Int] -> Maybe Int
solve n xs = go 0 $ troubleSort xs
  where
    go !ix (x:y:xs)
      | x <= y = go (ix + 1)(y:xs)
      | otherwise = Just ix
    go _ _ = Nothing

troubleSort :: [Int] -> [Int]
troubleSort xs = merge (L.sort $ evenIndexList xs) (L.sort $ oddIndexList xs)

merge :: [Int] -> [Int] -> [Int]
merge (x:xs) (y:ys) = x : y : merge xs ys
merge xs ys         = xs ++ ys

evenIndexList :: [Int] -> [Int]
evenIndexList (x:_:xs) = x : evenIndexList xs
evenIndexList xs       = xs

oddIndexList :: [Int] -> [Int]
oddIndexList (_:xs) = evenIndexList xs
oddIndexList []     = []

troubleSortNaive :: [Int] -> [Int]
troubleSortNaive = go []
  where
    go stock (x:y:z:xs)
      | x > z = go [] $ reverse stock ++ z : y : x : xs
      | otherwise = go (x:stock) (y:z:xs)
    go stock xs = reverse stock ++ xs

runGCJ :: IO () -> IO ()
runGCJ main_ = do
    t <- readLn
    forM_ [1..t] $ \i -> do
        putStr $ "Case #" ++ shows i ": "
        main_
