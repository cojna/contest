main :: IO ()
main = do
    [sx, sy, tx, ty] <- map read.words <$> getLine
    putStr $ 'L' : replicate (ty - sy + 1) 'U' ++ replicate (tx - sx + 1) 'R' ++ "D"
    putStr $ 'R' : replicate (ty - sy + 1) 'D' ++ replicate (tx - sx + 1) 'L' ++ "U"
    putStr $ replicate (ty - sy) 'U' ++ replicate (tx - sx) 'R'
    putStr $ replicate (ty - sy) 'D' ++ replicate (tx - sx) 'L'
    putStrLn ""
