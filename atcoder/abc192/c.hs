import Data.List

main :: IO ()
main = do
    [n, k] <- map read.words <$> getLine
    print $ iterate' f n !! k

f :: Int -> Int
f x = g1 x - g2 x
    where
        g1 = read . sortBy (flip compare) . show
        g2 = read . sort . show
