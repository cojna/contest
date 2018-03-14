main :: IO ()
main = do
    _  <- getLine
    xs <- map read . words <$> getLine
    print . length . takeWhile (all even) $ iterate (map (`div` 2)) xs
