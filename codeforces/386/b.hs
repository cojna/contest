{-# LANGUAGE BangPatterns #-}

main = do
  n <- readLn
  cs <- getLine
  putStrLn $ solve n cs

solve :: Int -> String -> String
solve 1 [c] = [c]
solve 2 [c, d] = [c, d]
solve n cs = go (even n) [] [] cs
  where
    go True ls rs (c:d:cs) = go True (c:ls) (d:rs) cs
    go False ls rs (c:cs) = go True (c:ls) rs cs
    go _ ls rs [] = ls ++ reverse rs