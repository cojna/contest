import           Data.List

main :: IO ()
main = do
  [n, t] <- map read.words <$> getLine
  xs <- map read.words <$> getLine
  print $ solve t xs

solve :: Int -> [Int] -> Int
solve t xs = foldl' step 0 xs
  where
    step acc x = head [a | a<-[acc..],mod a t == x]
