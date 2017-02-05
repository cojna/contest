import           Data.Bool
main=getContents>>=putStrLn.bool"NO""YES".even.length.filter odd.tail.map read.words
