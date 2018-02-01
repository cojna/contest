{-# OPTIONS_GHC -O2 #-}
import           Data.Char
main=readLn>>=print.f
f :: Int -> Int
f k=[x|x<-[1..],sum(map digitToInt(show x))==10]!!(k-1)
