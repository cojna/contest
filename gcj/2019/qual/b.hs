import           Control.Monad
import           Control.Applicative
import           Data.Fixed
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Debug.Trace

main :: IO ()
main = runGCJ $ do
    n <- readLn :: IO Int
    cs <- getLine
    putStrLn $ solve cs

solve :: String -> String
solve cs = map f cs
  where
    f 'E' = 'S'
    f 'S' = 'E'
    f c = undefined

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
