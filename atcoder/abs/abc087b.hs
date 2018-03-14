main :: IO ()
main = do
    [a, b, c, x] <- map read . words <$> getContents
    print $ sum [1|i<-[0..a], j<-[0..b], k<-[0..c], 500 * i + 100 * j + 50 * k == x]
