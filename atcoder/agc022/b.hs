main :: IO ()
main = readLn >>= putStrLn.unwords.map show.solve

lim :: Int
lim = 30000

solve :: Int -> [Int]
solve 3 = [2, 3, 25]
solve n
  | even n = take n xs
  | otherwise = (lim:).take(n-1) $ xs
  where
    xs = [2,3,4,9] ++ concat[[6*k+2, 6*k+4]|k<-[1..div lim 6 - 1]] ++ [15,21..lim] ++ [6,12..lim]
