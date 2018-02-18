import           Control.Applicative
import           Control.Monad
main = do
  [xs, ys, zs] <- replicateM 3 $ map read.words <$> getLine
  let g[x,y,z] = x==y && y==z
  let f xs ys= g $ zipWith (-) xs ys
  if f xs ys && f ys zs
  then putStrLn "Yes"
  else putStrLn "No"
