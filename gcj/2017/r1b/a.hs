import           Control.Monad
import           Data.Fixed
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Debug.Trace

main :: IO ()
main = runGCJ $ do
    [d_, n] <- map read.words <$> getLine
    let d = fromIntegral d_
    ts <- replicateM n $ do
        [k, s] <- map read.words <$> getLine
        return $ (d - k) / s
    print $ d / maximum ts

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
