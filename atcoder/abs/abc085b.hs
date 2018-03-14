import           Data.List

main :: IO ()
main = do
    _ <- getLine
    xs <- map read . words <$> getContents :: IO [Int]
    print . length . group $ sort xs
