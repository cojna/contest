main :: IO ()
main = do
    n <- readLn :: IO Int
    xs <- map read.words <$> getContents
    flip mapM_ (zip (tail xs) xs) $ \(x, y) -> do
        case compare x y of
            LT -> putStrLn $ unwords ["down", show $ y - x]
            EQ -> putStrLn "stay"
            GT -> putStrLn $ unwords ["up", show $ x - y]
