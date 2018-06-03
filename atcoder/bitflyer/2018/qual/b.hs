main :: IO ()
main = do
  [a,b,n] <- map read.words <$> getLine
  xs <- getLine
  mapM_ print $ solve a b xs

solve :: Int -> Int -> String -> [Int]
solve a b ('S':cs)
  | a > 0 = solve (a-1) b cs
  | otherwise = solve a b cs
solve a b ('C':cs)
  | b > 0 = solve a (b-1) cs
  | otherwise = solve a b cs
solve a b ('E':cs)
  | a > b = solve (a-1) b cs
  | b > a = solve a (b-1) cs
  | a > 0 = solve (a-1) b cs
  | otherwise = solve a b cs
solve a b _ = [a, b]
