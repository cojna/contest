import           Data.Char

main :: IO ()
main = do
    [n, a, b] <- map read . words <$> getLine
    print $ sum [x | x <- [1..n], let s = digitSum x, a<= s, s <= b]

digitSum :: Int -> Int
digitSum x = sum . map digitToInt $ show x
