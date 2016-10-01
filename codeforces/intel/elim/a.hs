main :: IO ()
main = do
  n <- readLn :: IO Int
  [h1,h2,':',m1,m2] <- getLine
  let hh = read[h1, h2] :: Int
  let mm = read[m1, m2] :: Int
  let hh' = if n == 12 && 1<=hh && hh<=12 then [h1, h2]
            else if n == 12 && h2 == '0' then ['1', h2]
            else if n == 12 then ['0', h2]
            else if 0<=hh && hh<24 then [h1, h2]
            else ['0', h2]
  let mm' = if 0 <= mm && mm <60 then [m1,m2] else ['0',m2]
  putStrLn $ hh' ++ ":" ++ mm'
