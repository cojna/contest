main :: IO ()
main = do
    [x, y] <- map read.words <$> getLine
    if y == 0
    then putStrLn "ERROR"
    else do
        putStrLn $ concat
            [ show $ div (x * 100) y `div` 100
            , "."
            , tail.show $ 100 + (div (x * 100) y `mod` 100)
            ]
