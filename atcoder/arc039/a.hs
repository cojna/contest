main :: IO ()
main = do
    [a, b] <- map read.words <$> getLine
    print $ solve a b

solve :: Int -> Int -> Int
solve a b = max (maxA - b) (a - minB)
  where
    maxA = case show a of
        ['9','9',_] -> 999
        ['9',_,x] -> read $ "99"++[x]
        [_,x,y] -> read $ '9':[x,y]
    minB = case show b of
        ['1','0',x] -> 100
        ['1',_,x] -> read $ "10"++[x]
        [_,x,y] -> read $ '1':[x,y]
