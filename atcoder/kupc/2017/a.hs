import           Control.Applicative
import           Data.List
main = do
  [_,k] <- map read.words <$> getLine
  xs <- map read.words <$> getLine
  print $ solve k xs

solve k xs = go k 0 $ sortBy (flip compare) xs
 where
  go acc res (x:xs)
    | acc <= 0 = res
    | otherwise = go (acc - x) (res + 1) xs
  go acc res []
    | acc <= 0 = res
    | otherwise = -1
