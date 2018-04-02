import           Control.Monad
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
       main_
