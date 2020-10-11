import Data.Bool

main :: IO ()
main = do
    [a,b,c,d] <- map read.words <$> getLine
    putStrLn . bool "No""Yes"$ solve a b c d

solve a b c d
    = any p [a, b, c, d]
    || any p [a + b, a + c, a + d, b + c, b + d, c + d]
  where
    p x = 2 * x == s
    s = a + b + c + d