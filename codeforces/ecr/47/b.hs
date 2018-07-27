{-# LANGUAGE ViewPatterns #-}

import           Data.Char
import           Data.List

main :: IO ()
main=getLine>>=putStrLn.concatMap show.solve.map digitToInt

solve :: [Int] -> [Int]
solve (span (<2) -> (xs, [])) = sort xs
solve (span (<2) -> (xs, ys)) = sort xs ++ filter (==1) ys ++ filter(/=1) ys
