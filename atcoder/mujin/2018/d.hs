{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}

import qualified Data.IntSet as IS
import           Debug.Trace

main :: IO ()
main = do
    [n, m] <- map read.words <$> getLine :: IO [Int]
    print $ solve n m

solve :: Int -> Int -> Int
solve n m = length . filter (`IS.member` table) $ pair <$> [1..n] <*> [1..m]

limX, limY :: Int
limX = 999
limY = 999

table :: IS.IntSet
table = go IS.empty IS.empty $ pair <$> [1..limX] <*> [1..limY]
  where
    go !terminate !nonTerminate (xy:xys) = go terminate' nonTerminate' xys
      where
        (terminate', nonTerminate') = simulate IS.empty xy
        simulate !visited p@(unPair -> (x, y))
            | IS.member p terminate = (IS.union terminate visited, nonTerminate)
            | IS.member p nonTerminate = (terminate, IS.union visited nonTerminate)
            | IS.member p visited = (terminate, IS.union visited nonTerminate)
            | x == 0 || y == 0 = (IS.union terminate visited, nonTerminate)
            | otherwise = simulate (IS.insert p visited) (step p)
    go _ nonTerminate []                   = nonTerminate
{-# NOINLINE table #-}

rev :: Int -> Int
rev x
    | x > 99 = rem x 10 * 100 + (rem x 100 `quot` 10) * 10 + quot x 100
    | x > 9 = rem x 10 * 10 + quot x 10
    | otherwise = x

type Pair = Int

unPair xy = quotRem xy 1000
pair x y = x * 1000 + y

step :: Pair -> Pair
step (unPair -> (x, y))
    | x == 0 || y == 0 = pair x y
    | x < y = f (rev x) y
    | otherwise = f x (rev y)
  where
    f nx ny
        | nx < ny = pair nx (ny - nx)
        | otherwise = pair (nx - ny) ny
