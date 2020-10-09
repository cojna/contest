main :: IO ()
main = do
    [h, w] <- map read.words <$> getLine
    print $ solve h w

solve :: Int -> Int -> Int
solve 1 w = 1
solve h 1 = 1
solve h w = div (h + 1) 2 * div (w + 1) 2 + div h 2 * div w 2
