import Data.Ratio
main :: IO ()
main = do
    n <- readLn :: IO Int
    xs <- map read.words <$> getLine :: IO [Integer]
    let res :: Double
        res = fromRational $ recip $ sum[1 % x|x<-xs]
    print res