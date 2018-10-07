import           Control.Monad

main :: IO ()
main = do
    n <- readLn
    xyhs <- replicateM n $ do
        [x, y, h] <- map read.words <$> getLine
        return (x, y, h)
    case solve xyhs of
        (cx, cy, h) -> putStrLn . unwords $ map show [cx, cy, h]

solve :: [(Int, Int, Int)] -> (Int, Int, Int)
solve xyhs = head $ do
    cx <- [0..100]
    cy <- [0..100]
    let hs = [h + abs(x-cx) + abs(y-cy) | (x,y,h)<-xyhs, h > 0]
    let ub = minimum $ maxBound:[abs(x-cx) + abs(y-cy) | (x,y,0)<-xyhs]
    case (hs, ub) of
        ([], 1) -> return (cx, cy, 1)
        ([], _) -> undefined
        (h:hs, ub)
          | all(==h)hs, h <= ub -> return (cx, cy, h)
          | otherwise -> []
