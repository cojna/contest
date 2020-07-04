{-# LANGUAGE BangPatterns #-}

import Debug.Trace

main :: IO ()
main = do
    k <- readLn
    [x, y] <- map read.words <$> getLine
    case solve k x y of
        Nothing -> print (-1)
        Just ans | validate k ans -> do
            print $ length ans
            putStr $ concat[shows x$' ':shows y"\n"|(x,y)<-ans]

prop :: Int -> Int -> Int -> Bool
prop k x y = maybe True (validate k) $ solve k x y

solve :: Int -> Int -> Int -> Maybe [(Int, Int)]
solve k x y
    | even k && odd (x + y) = Nothing
    | even k    = Just . map f' $ solveEven k (f (x', y'))
    | otherwise = Just . map f' $ solveOdd  k (f (x', y'))
  where
    (!x', !y') = rot45 (x, y)
    !sx' | x' == 0 = 1
         | otherwise = signum x'
    !sy' | y' == 0 = 1
         | otherwise = signum y'
    f (xx, yy) = (abs xx, abs yy)
    f' (xx', yy') = rot45' (sx' * xx', sy' * yy')

rot45 :: (Int, Int) -> (Int, Int)
rot45 (x, y) = (x - y, x + y)

rot45' :: (Int, Int) -> (Int, Int)
rot45' (x, y) = (div (x + y) 2, div (y - x) 2)

solveEven :: Int -> (Int, Int) -> [(Int, Int)]
solveEven k (gx, gy)
    | max gx gy == k = [(gx, gy)]
    | 0 < gx, gx < k, 0 < gy, gy < k = [(k, gy - k), (gx, gy)]
    | otherwise = go (0, 0)
  where
    go (!x, !y)
        | dx > 2 * k, dy > 2 * k = (x + k, y + k):go (x + k, y + k)
        | dx > 2 * k = (x + k, y):go (x + k, y)
        | dy > 2 * k = (x, y + k):go (x, y + k)
        | max dx dy == k = [(gx, gy)]
        | gx - x <= k = [(x + k, gy - k), (gx, gy)]
        | gy - y <= k = [(gx - k, y + k), (gx, gy)]
        | otherwise = [(gx - k, y + k), (gx, gy)]
      where
        !dx = gx - x
        !dy = gy - y

solveOdd :: Int -> (Int, Int) -> [(Int, Int)]
solveOdd k (gx, gy)
    | max gx gy == k = [(gx, gy)]
    | 0 < gx, gx < k, 0 < gy, gy < k, odd gx = [(k, -1), (gx + k, k - 1), (gx, gy)]
    | 0 < gx, gx < k, 0 < gy, gy < k = [(k, gy - k), (gx, gy)]
    | otherwise = go (0, 0)
  where
    go (!x, !y)
        | dx > 2 * k, dy > 2 * k = (x + k, y + k):go (x + k, y + k)
        | dx > 2 * k, y + 1 <= gy = (x + k, y + 1):go (x + k, y + 1)
        | dx > 2 * k = (x + k, y - 1):go (x + k, y - 1)
        | dy > 2 * k, x + 1 <= gx = (x + 1, y + k):go (x + 1, y + k)
        | dy > 2 * k = (x - 1, y + k):go (x - 1, y + k)
        | max dx dy == k = [(gx, gy)]
        | gx - x <= k, even dy = [(x + k, gy - k), (gx, gy)]
        | {- gy - y <= k, -} even dx = [(gx - k, y + k), (gx, gy)]
        | dx > k, odd dx = (x + k, y - 1) : go (x + k, y - 1)
        | dy > k, odd dy = (x - 1, y + k) : go (x - 1, y + k)
      where
        !dx = gx - x
        !dy = gy - y


dist :: (Int, Int) -> (Int, Int) -> Int
dist (x0, y0) (x1, y1) = abs (x1 - x0) + abs (y1 - y0)

validate :: Int -> [(Int, Int)] -> Bool
validate k xys = all (==k) $ zipWith dist ((0,0):xys) xys
