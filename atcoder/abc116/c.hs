import Data.Function
import Data.List

main :: IO ()
main = do
  _n <- readLn :: IO Int
  hs <- map read.words <$> getLine
  print $ solve hs

solve :: [Int] -> Int
solve hs
    | maximum hs == 0 = 0
    | otherwise = foldl' (+) m
        . map solve
        . groupBy ((==) `on` compare 0)
        $ map (subtract m) hs
  where
    m = minimum hs