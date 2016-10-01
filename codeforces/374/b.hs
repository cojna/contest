import           Control.Applicative
import           Control.Monad
import           Data.List

main :: IO ()
main = do
    [n, k] <- map read.words <$> getLine
    css <- replicateM n getLine
    pass <- getLine
    putStr.unwords.map show $ solve k pass css

solve :: Int -> String -> [String] -> [Int]
solve k pass css = [solveMin k pass css, solveMax k pass css]

solveMin, solveMax :: Int -> String -> [String] -> Int
solveMin k pass css = solve' k.(++[len]).takeWhile (<len) . sort $ map length css
  where
    len = length pass
solveMax k pass css = solve' k.takeWhile (<=len) . sort $ map length css
  where
    len = length pass

solve' :: Int -> [Int] -> Int
solve' k xs = go 0 0 xs
  where
    go miss res (x:xs)
      | miss == k = go 1 (res+6) xs
      | otherwise = go (miss+1) (res+1) xs
    go _ res [] = res
