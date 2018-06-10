import qualified Data.IntSet as IS

main :: IO ()
main = readLn >>= print . solve

solve :: Int -> Int
solve n = go 0 IS.empty $ IS.singleton 0
  where
    go depth used vs
        | IS.member n vs = depth
        | otherwise = go (depth + 1) (IS.union used nexts) $ nexts IS.\\ used
      where
        nexts = IS.unions . map next $ IS.toList vs
    next x = IS.unions
        [ IS.singleton $ x + 1
        , IS.fromList . takeWhile (<=n) . map (+x) $ iterate (*6) 6
        , IS.fromList . takeWhile (<=n) . map (+x) $ iterate (*9) 9
        ]
