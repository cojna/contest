import           Control.Monad
import           Data.Fixed
import           Data.List
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Debug.Trace

main :: IO ()
main = runGCJ $ do
    [n, r, 0, y, 0, b, 0] <- map read.words <$> getLine
    putStrLn $ maybe "IMPOSSIBLE" id $ solve n r y b


sf = sortBy (flip compare) . filter ((>0).fst)

isValid "" = False
isValid cs = head cs /= last cs && and (zipWith (/=) cs $ tail cs)

solve :: Int -> Int -> Int -> Int -> Maybe String
solve n r y b
--    | 2 * maximum [r, y, b] > n = Nothing
    | otherwise = go "" $ sf [(r, 'R'), (y, 'Y'), (b, 'B')]
  where
    go "" ((a,ac):rest) = go [ac] . sf $ (a-1,ac):rest
    go res@(prev:_) [(a,ac),(b,bc),(c,cc)]
      | a == b, b == c = case [ s|p<-permutations[ac,bc,cc],let s = ([1..a]>>p) ++ res, isValid s] of
          (res:_) -> Just res
          [] -> Nothing
    go res@(prev:_) [(a,ac),(b,bc)]
      | a == b = case [ s|p<-permutations[ac,bc],let s = ([1..a]>>p) ++ res, isValid s] of
          (res:_) -> Just res
          [] -> Nothing
    go res@(prev:_) ((a,ac):(b,bc):rest)
      | prev /= ac = go (ac:res) . sf $ (a-1,ac):(b,bc):rest
      | otherwise = go (bc:res) . sf $ (a,ac):(b-1,bc):rest
    go res@(prev:_) [(a,ac)]
      | prev /= ac = go (ac:res) . sf $ [(a-1,ac)]
      | otherwise = Nothing
    go "" [] = Nothing
    go [c] [] = Just [c]
    go cs []
      | head cs /= last cs = Just cs
      | otherwise = Nothing

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
