main :: IO ()
main = interact $ solve . read

solve :: Int -> String
solve 0 = "0"
solve x = reverse $ go x
  where
    go 0 = ""
    go x
        | (q, 0) <- divMod x (-2) = '0' : go q
        | (q, -1) <- divMod x (-2) = '1' : go (q + 1)
