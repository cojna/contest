main :: IO ()
main = do
    [n, m] <- map read.words <$> getLine
    putStr.concatMap (\(x, y) -> shows x $ ' ' : shows y "\n" ) $ solve n m

solve :: Int -> Int -> [(Int, Int)]
solve n m =
    let r = div (n - 1) 2
        diffs = map (n -) [1,3..r] ++ reverse [2,4..r]
    in take m $ zipWith (\i d -> (i, i + d)) [1..] diffs
