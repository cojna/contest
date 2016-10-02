import           Control.Applicative

main :: IO ()
main = do
  [_, a, b] <- map read.words <$> getLine :: IO [Int]
  xs <- map read.words <$> getContents
  print . length $ filter (\x -> x < a || b <= x) xs
