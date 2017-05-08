{-# LANGUAGE BangPatterns #-}

import           Data.List
import qualified Data.Set  as S

main = do
  [x, y, z] <- map read.words <$> getLine
  print $ solve x y z

solve :: Int -> Int -> Int -> Int
solve x y z = go S.empty 0 $ sort [x, y, z]
  where
    go s !acc xyz@[x, y, z]
      | any odd xyz = acc
      | S.member xyz s = -1
      | otherwise = go (S.insert xyz s) (acc+1) xyz'
      where
        x' = div (y + z) 2
        y' = div (z + x) 2
        z' = div (x + y) 2
        xyz' = sort[x', y', z']
