import           Data.List
main=getContents>>=print.g.partition(1==).map f.group.sort.tail.map read.words
f :: [Int] -> Int
f xs= 2 - length xs `mod` 2
g (xs,ys) = length xs + length ys - length ys `mod` 2
