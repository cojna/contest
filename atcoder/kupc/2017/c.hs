import           Control.Applicative
import           Data.Char

main = do
  a <- readLn
  cs <- map (\c -> ord c - ord 'a' + 1) <$> getLine
  putStrLn $ solve a cs

solve :: Int -> [Int] -> String
solve a cs = map f $ iterate (takeWhile(>0).go) cs !! 1000
  where
    f  c = chr $ c - 1 + ord 'a'
    go (x:0:xs) = x : go (0:xs)
    go (x:y:xs)
      | x + a <= 26 = go ((x+a):(y-1):xs)
      | otherwise = x : go (y:xs)
    go [x] = [x]
