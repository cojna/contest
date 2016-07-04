import Data.List
main = do
  n <- readLn
  if n > 26 then print (-1)
  else getLine >>= print.solve

solve :: String -> Int
solve cs = sum.map (pred.length).group$ sort cs
