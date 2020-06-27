main :: IO ()
main = do
  s <- getLine
  t <- getLine
  print . length . filter id$ zipWith (/=) s t