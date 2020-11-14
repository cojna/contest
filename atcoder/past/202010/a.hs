import Data.List

main :: IO ()
main = do
    xs <- map read.words <$> getLine :: IO [Int]
    putChar . snd $ sort (zip xs "ABC") !! 1
