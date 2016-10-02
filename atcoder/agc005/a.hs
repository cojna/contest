{-# LANGUAGE BangPatterns #-}

main :: IO ()
main = do
    cs <- getLine
    print $ solve $ map conv cs

conv 'S' = '('
conv 'T' = ')'
conv _ = undefined

solve :: String -> Int
solve cs = length $ go 0 0 cs
  where
    go open close (')':rest)
      | open > 0 = go (open-1) close rest
      | otherwise = go 0 (close+1) rest
    go open close ('(':rest) = replicate close ')' ++ go (open+1) 0 rest
    go open close [] = replicate close ')' ++ replicate open '('
