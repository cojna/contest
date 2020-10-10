main :: IO ()
main = do
    [t1, t2] <- map read.words <$> getLine
    [a1, a2] <- map read.words <$> getLine
    [b1, b2] <- map read.words <$> getLine
    let v1 = b1 - a1
    let v2 = b2 - a2
    putStrLn.maybe "infinity" show $ solve t1 t2 v1 v2

solve
    :: Integer
    -> Integer
    -> Integer
    -> Integer
    -> Maybe Integer
solve t1 t2 v1 v2
    | dx == 0 = Nothing
    | dx1 * dx >= 0 = Just 0
    | r > 0 = Just $ 2 * q + 1
    | otherwise = Just $ 2 * q
  where
    dx1 = t1 * v1
    dx2 = t2 * v2
    dx = dx1 + dx2
    (q, r) = divMod (abs dx1) (abs dx)