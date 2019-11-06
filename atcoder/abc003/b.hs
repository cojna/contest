main :: IO ()
main = do
    s <- getLine
    t <- getLine
    putStrLn $ solve s t

solve :: String -> String -> String
solve s t
    | go s t = "You can win"
    | otherwise = "You will lose"
  where
    go ('@':xs) ('@':ys) = go xs ys
    go ('@':xs) (y:ys) = elem y "atcoder" && go xs ys
    go (x:xs) ('@':ys) = elem x "atcoder" && go xs ys
    go (x:xs) (y:ys) = x == y && go xs ys
    go _ _ = True
