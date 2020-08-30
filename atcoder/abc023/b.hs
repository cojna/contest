main :: IO ()
main = do
    n <- readLn
    s <- getLine
    print . maybe (-1) id $ solve n s

solve :: Int -> String -> Maybe Int
solve n s
    | simulate res == s = Just res
    | otherwise = Nothing
  where
    res = div (n - 1) 2

simulate :: Int -> String
simulate n = snd $ iterate f (1, "b") !! n
  where
    f (i, s) = case rem i 3 of
        0 -> (i + 1, 'b':s++"b")
        1 -> (i + 1, 'a':s++"c")
        _ -> (i + 1, 'c':s++"a")
