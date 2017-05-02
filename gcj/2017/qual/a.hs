{-# LANGUAGE BangPatterns #-}

import           Control.Monad
import           Data.Fixed
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Debug.Trace

main :: IO ()
main = runGCJ $ do
    [cs, k] <-  words <$> getLine
    putStrLn . maybe "IMPOSSIBLE" show $ solve (read k) $ map (== '+') cs

solve :: Int -> [Bool] -> Maybe Int
solve k bs = go 0 bs
  where
    go !acc [] = Just acc
    go !acc bs = case span id bs of
        (ts, []) -> Just acc
        (ts, rest)
          | length rest >= k -> go (acc + 1) $ flipK k rest
          | otherwise -> Nothing

flipK :: Int -> [Bool] -> [Bool]
flipK k xs = map not ys ++ zs
  where
    (ys, zs) = splitAt k xs


runGCJ :: IO () -> IO ()
runGCJ main_ = do
    t <- readLn
    forM_ [1..t] $ \i -> do
        putStr $ "Case #" ++ shows i ": "
        traceIO $ shows i "/" ++ shows t ": "
        time <- flip (-) <$> getPOSIXTime <* main_ <*> getPOSIXTime
        traceIO $ (shows.msec) time "ms\n"

msec :: NominalDiffTime -> Int
msec s = let t = realToFrac s :: Milli in fromEnum t
