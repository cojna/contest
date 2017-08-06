import           Control.Applicative
import           Data.List.Split

main :: IO ()
main = do
  [h, w] <- map read.words <$> getLine :: IO [Int]
  _ <- getLine
  xs <- map read.words <$> getLine
  putStr.unlines.map(unwords.map show) $ solve h w xs

solve :: Int -> Int -> [Int] -> [[Int]]
solve h w xs = go.chunksOf w.concat $ zipWith replicate xs [1..]
  where
    go (xs:ys:xss) = xs:reverse ys:go xss
    go xss = xss
