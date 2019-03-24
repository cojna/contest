main :: IO ()
main = do
  n <- readLn
  case solve n of
      res -> do
          print $ length res
          putStr . unlines . map (unwords.map show) $ map (\(x, y) -> [x, y]) res

solve :: Int -> [(Int, Int)]
solve n
    | even n = [(x, y)|x<-[1..n], y<-[x+1..n], x + y /= n + 1]
    | otherwise = [(x, y)|x<-[1..n], y<-[x+1..n], x + y /= n || y == n ]
