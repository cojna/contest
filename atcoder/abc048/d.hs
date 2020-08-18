main :: IO ()
main = do
    cs <- getLine
    if solve cs
    then putStrLn "First"
    else putStrLn "Second"

solve :: String -> Bool
solve cs
    | head cs == last cs = even (length cs)
    | otherwise = odd (length cs)
