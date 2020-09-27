import Data.Maybe

main :: IO ()
main = do
    [n, k] <- map read.words <$> getLine
    xs <- map read.words <$> getContents
    print . maybe (-1) id $ solve k xs

solve :: Int -> [Int] -> Maybe Int
solve k xs
    = listToMaybe
    . map fst
    . filter ((<k) . snd)
    . zip [3..]
    $ zipWith3 (\x y z -> x + y + z) xs (drop 1 xs) (drop 2 xs)
