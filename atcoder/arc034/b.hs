main :: IO ()
main = do
    n <- readLn
    let res = [x|x<-[max 0 (n - 999)..n+999], x + digitSum x == n]
    print $ length res
    putStr.unlines $ map show res

digitSum :: Int -> Int
digitSum n = sum[read[c]|c<-show n]

