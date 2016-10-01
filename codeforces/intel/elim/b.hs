import           Control.Applicative
import           Control.Monad
import           Data.Bool

main :: IO ()
main = do
  n <- readLn
  xs <- map read.words <$> getLine
  css <- replicateM n getLine
  putStrLn . bool "NO" "YES" .and $ zipWith (==) xs [length$filter(`elem`"aeiouy")cs|cs<-css]
