module Main where

import qualified Data.List as L
import qualified Data.Set as S

main :: IO ()
main = do
  cs <- getLine
  print $ solve cs

solve :: String -> Int
solve cs =
  pred
    . S.size
    . S.fromList
    $ concatMap gen [1 .. 3]
  where
    gen :: Int -> [String]
    gen i =
      concatMap (mapM (: ".") . take i) $
        L.tails cs


