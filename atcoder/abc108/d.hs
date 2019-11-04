main :: IO ()
main = do
  l <- readLn
  print $ solve l

data Graph = Graph !Int ![(Int, Int, Int)]

instance Show Graph where
    show (Graph n es)
        = shows n " " ++ shows (length es) "\n"
        ++ concatMap (\(x, y, z) ->
            (shows x " " ++ shows y " " ++ shows z "\n")) es

solve :: Int -> Graph
solve n = go n
  where
    go 1 = Graph 2 [(1, 2, 0)]
    go 2 = Graph 2 [(1, 2, 1), (1, 2, 0)]
    go 3 = Graph 2 [(1, 2, 2), (1, 2, 1), (1, 2, 0)]
    go x = case quotRem x 2 of
        (q, 0) -> let Graph l es = go q
                  in Graph (l + 1) ((l,l+1,q):(l,l+1,0):es)
        (q, 1) -> let Graph l es = go q
                  in Graph (l + 1) ((1,l+1,x-1):(l,l+1,q):(l,l+1,0):es)
