{-# LANGUAGE BangPatterns #-}
import           Control.Monad
import           Control.Applicative
import qualified Data.ByteString.Char8 as C
import           Data.Char
import           Data.Fixed
import qualified Data.List             as L
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Data.Tuple
import           Debug.Trace

main :: IO ()
main = runGCJ $ do
    [n, l] <- map read.words <$> getLine :: IO [Int]
    xs <- L.unfoldr (C.readInteger.C.dropWhile isSpace) <$> C.getLine
    putStrLn $ solve xs

solve :: [Integer] -> String
solve xs@(x:_) = [c | Just c<-flip lookup table <$> ps]
  where
    (_:ls, y:rs) = span (== x) xs
    !g = gcd x y
    !ps = init (scanr div (x `div` g) ls) ++ L.scanl' (flip div) (x `div` g) (x:y:rs)
    !table = flip zip ['A'..] . map head . L.group $ L.sort ps

smallPrimes :: [Integer]
smallPrimes = 2 : [ n | n<-[3,5..46337], all ((>0).rem n) $ takeWhile (\x->x*x<=n) smallPrimes]

defaultTable :: [(Char, Integer)]
defaultTable = zip ['A'..'Z'] $ tail smallPrimes

encrypt :: String -> [Integer]
encrypt cs = zipWith (*) ps (p:ps)
  where
    (p:ps) = [c | Just c<-flip lookup defaultTable <$> cs]

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