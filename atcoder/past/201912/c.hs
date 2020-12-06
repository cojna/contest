import Data.List

main :: IO ()
main = do
    xs <- map read.words <$> getLine :: IO [Int]
    print $ sortOn negate xs !! 2
