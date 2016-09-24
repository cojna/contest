import           Data.List

s = [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1]
ss = [0..9]>>s

main = interact $ solve.map read.words

solve :: [Int] -> String
solve (l:xs) = case yss of
    [ys] | head (drop l ys) > x -> "UP\n"
         | otherwise -> "DOWN\n"
    _ -> "-1\n"
  where
    yss = filter(xs `isPrefixOf`).take 30 $ tails ss
    x = last xs
