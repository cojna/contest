import           Data.List

main :: IO ()
main = do
    _ <- getLine
    xs <- map read . words <$> getLine
    print . sum . zipWith (*) (cycle [1, -1]) $ sortBy (flip compare) xs
