main :: IO ()
main = do
    n <- readLn
    xs <- map read.words <$> getLine
    print . maybe (-1) id $ solve n xs

solve :: Int -> [Int] -> Maybe Int
solve n xs0
    | r == 0 = Just $ go 0 0 xs0
    | otherwise = Nothing
  where
    (avg, r) = quotRem (sum xs0) n
    go res acc (x:xs)
        | acc + x /= avg = go (res + 1) (acc + x - avg) xs
        | otherwise = go res 0 xs
    go res _ [] = res
