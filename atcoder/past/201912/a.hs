import Data.Char

main :: IO ()
main = do
    cs <- getLine
    if all isDigit cs
    then print $ 2 * read cs
    else putStrLn "error"
