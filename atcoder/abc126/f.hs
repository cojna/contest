main :: IO ()
main = do
  [m, k] <- map read.words <$> getLine
  putStrLn . maybe "-1" (unwords.map show) $ solve m k

solve :: Int -> Int -> Maybe [Int]
solve m 0 = Just $ concatMap (\i -> [i, i]) [0..2^m-1]
solve 1 _ = Nothing
solve m k
    | k < 2 ^ m = Just $ k : reverse xs ++ 0:k:0:xs
    | otherwise = Nothing
  where
    xs = [1..k-1] ++ [k+1..2^m-1]