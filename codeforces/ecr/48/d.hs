import           Control.Applicative
import           Data.Bits
import qualified Data.Foldable       as F

main :: IO ()
main = do
    [n, m] <- map read.words <$> getLine
    xs <- map read.words <$> getLine
    ys <- map read.words <$> getLine
    case solve n m xs ys of
        Just res -> do
            putStrLn "YES"
            putStr.unlines.map(unwords.map show) $ res
        Nothing -> putStrLn "NO"

solve :: Int -> Int -> [Int] -> [Int] -> Maybe [[Int]]
solve n m (x:xs) (y:ys)
    | F.foldl' xor x xs == F.foldl' xor y ys = Just $ hd : tl
    | otherwise = Nothing
  where
    hd = F.foldl' xor y xs : ys
    tl = zipWith (:) xs . repeat $ replicate (m - 1) 0
