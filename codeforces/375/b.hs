import           Data.List

main :: IO ()
main = getLine >> getLine >>= putStrLn . unwords . map show . solve

solve :: String -> [Int]
solve cs = go False 0 [] "" cs
  where
    go inParen resOut resIn w ('_':cs)
      | inParen = go inParen resOut (w:resIn) "" cs
      | otherwise = go inParen (max resOut $ length w) resIn "" cs
    go inParen resOut resIn w ('(':cs)
      | inParen = undefined
      | otherwise = go True (max resOut $ length w) resIn "" cs
    go inParen resOut resIn w (')':cs)
      | inParen = go False resOut (w:resIn) "" cs
      | otherwise = undefined
    go inParen resOut resIn w (c:cs) = go inParen resOut resIn (c:w) cs
    go _ resOut resIn w "" = [max resOut $ length w, length $ filter (not.null) resIn]
