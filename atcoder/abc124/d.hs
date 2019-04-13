{-# LANGUAGE BangPatterns #-}

import qualified Data.List as L

main :: IO ()
main = do
    [n, k] <- map read.words <$> getLine
    cs <- getLine
    print $ solve k cs

solve :: Int -> String -> Int
solve k cs = go 0 0 0 [] [] $ L.group cs
  where
    go !res !acc !num0 fs rs (l@('1':_):css)
        = go (max res acc') acc' num0 fs (l:rs) css
      where
        acc' = acc + length l
    go !res !acc !num0 fs rs (l@('0':_):css)
        | num0 + 1 <= k = go (max res acc') acc' (num0 + 1) fs (l:rs) css
      where
        acc' = acc + length l
    go !res !acc !num0 [] rs (l@('0':_):css) = go res acc num0 (reverse rs) [] (l:css)
    go !res !acc !num0 (f@('0':_):fs) rs (l@('0':_):css)
        = go (max res acc') acc' num0 fs (l:rs) css
      where
        acc' = acc - length f + length l
    go !res !acc !num0 (f@('1':_):fs) rs (l@('0':_):css)
        = go res acc' num0 fs rs (l:css)
      where
        acc' = acc - length f
    go res _ _ _ _ [] = res

