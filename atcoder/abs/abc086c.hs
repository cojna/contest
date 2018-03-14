import           Data.Bool

main :: IO ()
main = do
    _ <- getLine
    txys <- map read . words <$> getContents
    putStrLn . bool "No" "Yes" $ solve txys

solve :: [Int] -> Bool
solve txys = go 0 0 0 txys
  where
    go pt px py (t:x:y:rest)
        | dx + dy  <= dt, even (dt - dx - dy) = go t x y rest
        | otherwise = False
      where
        dt = abs $ t - pt
        dx = abs $ x - px
        dy = abs $ y - py
    go _ _ _ _ = True
