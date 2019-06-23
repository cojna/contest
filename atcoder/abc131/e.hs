main :: IO ()
main = do
    [n, k] <- map read.words <$> getLine
    case solve n k of
        Nothing -> print (-1)
        Just es -> do
            print $ length es
            putStr $ unlines [shows x " " ++ show y|(x,y)<-es]

solve :: Int -> Int -> Maybe [(Int, Int)]
solve n k
    | k > (n - 1) * (n - 2) `div` 2 = Nothing
    | otherwise = Just $ base ++ take need [(i, j)|i<-[1..n-1],j<-[i+1..n-1]]
  where
    base = map ((,) n) [1..n-1]
    need = (n - 1) * (n - 2) `div` 2 - k
