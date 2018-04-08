{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns #-}

import           Control.Monad
import           Debug.Trace

main :: IO ()
main = runGCJ $ do
    [d, cs] <- words <$> getLine
    putStrLn . maybe "IMPOSSIBLE" show $ solve (read d) cs

solve :: Int -> String -> Maybe Int
solve d cs = go 0 $ map (== 'S') cs
  where
    go acc bs
      | damage bs <= d = Just acc
      | Just bs' <- hack bs = go (acc + 1) bs'
      | otherwise = Nothing

hack :: [Bool] -> Maybe [Bool]
hack bs = go [] $ reverse bs
  where
    go trail (True:False:rest) = Just $ reverse rest ++ True : False : trail
    go trail (b:rest)          = go (b:trail) rest
    go _ []                    = Nothing

damage :: [Bool] -> Int
damage bs = go 0 1 bs
  where
    go !d !l (True:rest) = go (d + l) l rest
    go d l (False:rest)  = go d (l * 2) rest
    go d _ []            = d

runGCJ :: IO () -> IO ()
runGCJ main_ = do
    t <- readLn
    forM_ [1..t] $ \i -> do
        putStr $ "Case #" ++ shows i ": "
        main_
