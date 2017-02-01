import           Control.Applicative
import           Data.Bool
import           Data.List
import           Debug.Trace

main :: IO ()
main = do
  [_, l] <- map read.words <$> getLine
  xs <- map read.words <$> getLine
  ys <- map read.words <$> getLine
  putStrLn . bool"NO""YES"$ solve l xs ys

solve _ [_] [_] = True
solve l xs ys = any (flip elem yss) xss
  where
    xss = [f x dxs|x<-[0..l-1]]
    yss = [f y dys|y<-[0..l-1]]
    f a ds = scanl (\x y -> (x + y) `rem` l) a ds
    dxs = zipWith(-)(tail xs) xs
    dys = zipWith(-)(tail ys) ys
