{-# LANGUAGE BangPatterns #-}

import Control.Monad

main :: IO ()
main = do
    [n, m] <- map read.words <$> getLine
    putStrLn.unwords.map show $ solve n m

solve :: Int -> Int -> [Int]
solve n m = case anss of
    [] -> [-1, -1, -1]
    (ans:_) -> ans
  where
    anss :: [[Int]]
    anss = do
        !z <- [0..n]
        let !y = m - 2 * n - 2 * z
        guard $ 0 <= y && y <= n
        let !x = n - y - z
        guard $ 0 <= x && x <= n
        return [x, y, z]
