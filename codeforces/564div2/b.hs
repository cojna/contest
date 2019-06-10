main :: IO ()
main = do
  n <- readLn
  let m = head [x | x<-[1..n],n<=2*x-1]
  print m
  flip mapM_ [1..m] $ \i -> do
    putStr "1 " >> print i
  flip mapM_ (take (n-m) [2..]) $ \i -> do
    putStrLn $ shows i " " ++ show m
