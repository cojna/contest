import Control.Monad

main=readLn>>=print.solve

solve :: Int -> Int
solve n = solve3x5x5 n + solve3x25 n + solve5x15 n + solve75 n
solve3x5x5 n = length $ do
    x <- filter (has n 4) smallPrimes
    y <- filter (has n 4) smallPrimes
    guard $ x < y
    z <- filter (has n 2) smallPrimes
    guard $ z /= x && z /= y
    return ()
solve3x25 n = length $ do
    x <- filter (has n 2) smallPrimes
    y <- filter (has n 24) smallPrimes
    guard $ x /= y
    return ()
solve5x15 n = length $ do
    x <- filter (has n 4) smallPrimes
    y <- filter (has n 14) smallPrimes
    guard $ x /= y
    return ()
solve75 n = length $ do
    x <- filter (has n 74) smallPrimes
    return ()

has :: Int -> Int -> Int -> Bool
has n k x = k <= sum[quot n (x ^ i)|i<-takeWhile((<=n).(x^))[1..]]

smallPrimes :: [Int]
smallPrimes = 2 : [ n | n<-[3,5..97], all ((>0).rem n) $ takeWhile (\x->x*x<=n) smallPrimes]
