main :: IO ()
main = do
    [n, a, b, c] <- map read.words <$> getLine
    ls <- map read.words <$> getContents
    print $ solve n a b c ls

solve :: Int -> Int -> Int -> Int -> [Int] -> Int
solve n a b c ls = minimum $ do
    gr <- mapM (const [0..3]) ls
    ga@(_:_) <- return . map snd . filter ((== 0).fst) $ zip gr ls
    gb@(_:_) <- return . map snd . filter ((== 1).fst) $ zip gr ls
    gc@(_:_) <- return . map snd . filter ((== 2).fst) $ zip gr ls
    let [la, lb, lc] =  map length [ga, gb, gc]
    let [sa, sb, sc] = map sum [ga, gb, gc]
    return $! (la + lb + lc - 3) * 10 + abs (sa - a) + abs (sb - b) + abs (sc - c)
