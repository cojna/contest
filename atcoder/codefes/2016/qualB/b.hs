{-# LANGUAGE BangPatterns #-}

import           Control.Applicative
import           Data.Bool

main :: IO ()
main = do
  [n, a, b] <- map read.words <$> getLine
  cs <- getLine
  putStr.unlines.map(bool"No""Yes")$solve a b cs

solve :: Int -> Int -> String -> [Bool]
solve a0 b0 cs = go 0 0 cs
  where
    go !ab !b ('a':cs)
      | ab < a0 + b0 = True : go (ab+1) b cs
      | otherwise = False : go ab b cs
    go !ab !b ('b':cs)
      | ab < a0 + b0, b < b0 = True : go (ab+1) (b+1) cs
      | otherwise = False : go ab b cs
    go !ab !b ('c':cs) = False : go ab b cs
    go _ _ (_:_) = undefined
    go _ _ [] = []
