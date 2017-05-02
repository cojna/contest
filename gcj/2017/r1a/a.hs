import           Control.Monad
import           Data.Fixed
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Debug.Trace

main :: IO ()
main = runGCJ $ do
    [r, c] <- map read.words <$> getLine
    css <- replicateM r getLine
    mapM_ putStrLn $ solve r c css

solve :: Int -> Int -> [String] -> [String]
solve r c css = reverse.go (replicate c '?') $ reverse $ go (replicate c '?') css
  where
    go prev (cs:css)
      | all (== '?') cs = prev : go prev css
      | c <- head $ filter(/='?') cs
      , cur <- tail $ scanl step c cs = cur : go cur css
    go prev [] = []

    step c '?' = c
    step _ c = c

runGCJ :: IO () -> IO ()
runGCJ main_ = do
    t <- readLn
    forM_ [1..t] $ \i -> do
        putStrLn $ "Case #" ++ shows i ": "
        traceIO $ shows i "/" ++ shows t ": "
        time <- flip (-) <$> getPOSIXTime <* main_ <*> getPOSIXTime
        traceIO $ (shows.msec) time "ms\n"

msec :: NominalDiffTime -> Int
msec s = let t = realToFrac s :: Milli in fromEnum t
