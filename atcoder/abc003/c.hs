import qualified Data.List as L

main :: IO ()
main = do
    [n, k] <- map read.words <$> getLine
    rs <- map read.words <$> getLine
    print $ solve k rs

solve :: Int -> [Double] -> Double
solve k xs
    = L.foldl' (\acc r -> (acc + r) * 0.5) 0.0
    . reverse
    . take k
    $ L.sortBy (flip compare) xs
