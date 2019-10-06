main :: IO ()
main = do
    [h, w] <- map read.words <$> getLine
    print $ solve h w

solve :: Int -> Int -> Int
solve h w
    | mod h 3 == 0 || mod w 3 == 0 = 0
    | otherwise = minimum resH `min` minimum resW
  where
    f x y z
        | even $ x * y = abs $ z - (x * y) `quot` 2
        | s <- (x * y - min x y) `quot` 2
            = maximum [abs (s - z), abs (x * y - s - z), min x y]
    resH = [f h (w - i) (h * i)| i <-[1..w-1]]
    resW = [f w (h - i) (w * i)| i <-[1..h-1]]
