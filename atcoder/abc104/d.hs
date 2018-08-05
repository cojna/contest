{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}

#define MOD 1000000007

main :: IO ()
main = do
    cs <- getLine
    print $ solve cs

solve :: String -> Int
solve cs = go 1 0 0 0 cs
  where
    go !e !a !ab !abc ('A':rest) = go e (rem (a + e) MOD) ab abc rest
    go !e !a !ab !abc ('B':rest) = go e a (rem (a + ab) MOD) abc rest
    go !e !a !ab !abc ('C':rest) = go e a ab (rem (ab + abc) MOD) rest
    go !e !a !ab !abc ('?':rest) = go (rem (e * 3) MOD) (rem (e + a * 3) MOD) (rem (a + ab * 3) MOD) (rem (ab + abc * 3) MOD) rest
    go _ _ _ abc [] = abc `rem` MOD
