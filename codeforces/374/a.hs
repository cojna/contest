import           Data.List

main :: IO ()
main = do
  n <- readLn :: IO Int
  cs <- getLine
  let res = solve cs
  print $ length res
  mapM_ print res

solve :: String -> [Int]
solve cs = [length xs|xs@('B':_)<-group cs]
