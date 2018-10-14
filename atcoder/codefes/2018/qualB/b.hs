import           Control.Applicative

main :: IO ()
main = do
    [n, x] <- map read.words <$> getLine
    xys <- parse.map read.words <$> getContents
    print $ solve x xys

parse (x:y:xs) = (x, y) : parse xs
parse _        = []

solve :: Int -> [(Int, Int)] -> Int
solve x xys = sum (zipWith (*) xs ys) + m * x
  where
    m = maximum $ map snd xys
    (xs, ys) = unzip xys
