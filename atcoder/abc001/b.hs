main :: IO ()
main = do
  m <- readLn
  putStrLn $ vv m

vv :: Int -> String
vv m
    | m < 100 = "00"
    | m < 1000 = "0" ++ show (div m 100)
    | m <= 5000 = show (div m 100)
    | m < 6000 = undefined
    | m <= 30000 = show (div m 1000 + 50)
    | m < 35000 = undefined
    | m <= 70000 = show (div (div m 1000 - 30) 5 + 80)
    | otherwise = "89"
