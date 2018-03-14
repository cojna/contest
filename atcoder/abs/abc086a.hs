import           Data.Bool

main :: IO ()
main = getLine >>= putStrLn . bool "Even" "Odd" . odd . product . map read . words
