import           Control.Monad
import           Control.Applicative
import           Data.Fixed
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Debug.Trace

main :: IO ()
main = runGCJ $ do
    cs <- getLine
    let (a, b) = solve cs
    putStrLn $ unwords [a, b]

solve :: String -> (String, String)
solve cs = (map f cs, map g cs)
  where
    f '4' = '2'
    f c = c
    g '4' = '2'
    g _ = '0'

-------------------------------------------------------------------------------

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
