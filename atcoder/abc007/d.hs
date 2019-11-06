main :: IO ()
main = do
    [a, b] <- map read.words <$> getLine
    print $ solve a b

solve :: Int -> Int -> Int
solve a b = freq b - freq (a - 1)

freq :: Int -> Int
freq x = go $ show x
  where
    go [x]
        | x < '4' = 0
        | x < '9' = 1
        | otherwise = 2
    go ('9':xs) = read xs + 1 + 9 * 10 ^ length xs - 8 * 8 ^ length xs
    go ('4':xs) = read xs + 1 + 4 * 10 ^ length xs - 4 * 8 ^ length xs
    go (x:xs)
        | x < '4' = read[x] * 10 ^ length xs - read[x] * 8 ^ length xs + go xs
        | otherwise = read[x] * 10 ^ length xs - (read[x] - 1) * 8 ^ length xs + go xs
