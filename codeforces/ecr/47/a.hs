import           Control.Applicative

main :: IO ()
main = do
    [n, m] <- map read.words <$> getLine :: IO [Int]
    cs <- map read.words <$> getLine
    bs <- map read.words <$> getLine
    print $ solve cs bs

solve :: [Int] -> [Int] -> Int
solve cs bs = go 0 cs bs
  where
    go res (c:cs) (b:bs)
      | c <= b = go (res + 1) cs bs
      | otherwise = go res cs (b:bs)
    go res _ _ = res
