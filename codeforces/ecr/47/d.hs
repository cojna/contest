import           Control.Applicative

main :: IO ()
main = do
    [n, m] <- map read.words <$> getLine
    case solve n m of
        Just res -> do
            putStrLn "Possible"
            putStr $ unlines [shows x " " ++ show y |(x,y)<-res]
        Nothing -> putStrLn "Impossible"

lim :: Int
lim = 1024

solve :: Int -> Int -> Maybe [(Int, Int)]
solve n m
    | m < n - 1 || length coprimes < m= Nothing
    | otherwise = Just $ take m coprimes
  where
    coprimes = map ((,)1) [2..n] ++ [(x, y)|x<-[2..min n lim], y<-[x+1..min n lim], gcd x y == 1]
