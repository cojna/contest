main :: IO ()
main = interact $ show.(\[n, x] -> solve n x).map read.words

solve :: Int -> Int -> Int
solve 0 0 = 0
solve 0 _ = 1
solve _ 0 = 0
solve _ 1 = 0
solve n x
    | x <= l = solve (n - 1) (x - 1)
    | otherwise = patty !! (n - 1) + 1 + solve (n - 1) (x - l - 1)
  where
    l = (len !! n) `div` 2

len :: [Int]
len = iterate (\x -> 2 * x + 3) 1

patty :: [Int]
patty = iterate (\x -> 2 * x + 1) 1
