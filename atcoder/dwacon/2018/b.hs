main=getLine>>=print.maybe(-1)id.solve

solve :: String -> Maybe Int
solve cs = go 0 cs
  where
    go 2525 cs = Nothing
    go res "" = Just res
    go res cs = go (res + 1) $ step cs

    step ('2':'5':xs) = step xs
    step (x:xs) = x : step xs
    step xs = xs
