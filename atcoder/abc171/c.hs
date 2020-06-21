import Debug.Trace

main :: IO ()
main = do
    n <- readLn
    putStrLn $ solve n

solve :: Integer -> String
solve x
  | x <= 26 = [['a'..] !! fromIntegral (x - 1)]
  | (q, r) <- divMod (x - 1) 26 = solve q ++ [['a'..] !! fromIntegral r]