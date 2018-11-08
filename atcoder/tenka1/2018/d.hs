import           Data.List
import           Data.Tuple

main :: IO ()
main = do
    n <- readLn :: IO Int
    case solve n of
        Just (k, mat) -> do
            putStrLn "Yes"
            print k
            putStr.unlines $ map (unwords.map show) mat
        Nothing -> putStrLn "No"

solve :: Int -> Maybe (Int, [[Int]])
solve 1 = Just (2, [[1, 1], [1, 1]])
solve n = case find (\k->k*(k-1)==2*n) [1..448] of
    Nothing -> Nothing
    Just k -> Just (k, map((k-1):).zipWith (++) tri $ transpose tri ++ [[]])
      where
        tri = snd $ mapAccumL ((swap.).flip splitAt) [1..n] [0..k-1]
