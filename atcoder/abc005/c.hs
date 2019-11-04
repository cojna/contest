import           Data.Bool
import qualified Data.List as L

main :: IO ()
main = do
    t <- readLn
    n <- readLn :: IO Int
    xs <- map read.words <$> getLine
    m <- readLn :: IO Int
    ys <- map read.words <$> getLine
    putStrLn . bool "no" "yes" $ solve t xs ys

solve :: Int -> [Int] -> [Int] -> Bool
solve t xs ys = go xs ys
  where
    go (x:xs) (y:ys)
        | x <= y && y <= x + t = go xs ys
        | otherwise = go xs (y:ys)
    go _ [] = True
    go _ _ = False