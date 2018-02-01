{-# LANGUAGE BangPatterns #-}
import           Control.Applicative
import           Control.Monad
import           Data.List
main = do
  [n, _, k]  <- map read.words <$> getLine
  css <- replicateM n getLine
  print $ solve k css

solve :: Int -> [String] -> Int
solve k css
  | k == 1 = sum $ map step css
  | otherwise =  sum (map step css) + sum (map step (transpose css))
  where
    step cs = sum [l - k + 1 | g@('.':_)<-group cs, let !l=length g, l >= k]
