{-# LANGUAGE BangPatterns #-}
import           Data.Char

main :: IO ()
main = do
  cs <- getLine
  k <- readLn
  putStrLn $ solve k cs

solve :: Int -> String -> String
solve 0 cs = cs
solve _ "" = ""
solve !k [c] = [change k c]
solve !k (c:cs)
  | cost c <= k = 'a' : solve (k - (cost c)) cs
  | otherwise = c : solve k cs

change :: Int -> Char -> Char
change k c | k >= 26 = change (k `rem` 26) c
change k c = (!!k).dropWhile (/=c) $ cycle['a'..'z']

cost :: Char -> Int
cost c = (123 - ord c) `rem` 26
