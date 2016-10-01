{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns #-}
import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString.Char8 as B
import qualified Data.IntSet           as IS

main :: IO ()
main = do
  n <- readLn :: IO Int
  xs <- map readInt.B.words <$> B.getLine
  putStrLn.unwords.map show $ solve xs

solve :: [Int] -> [Int]
solve xs = go $ IS.fromList xs
  where
    go set = case filter (`IS.notMember`set') ms of
        (y:_) -> go $ IS.insert y set'
        [] -> IS.toList set
      where
        (m, set') = IS.deleteFindMax set
        ms = tail.takeWhile (>0) $ iterate (`quot`2) m


readInt :: B.ByteString -> Int
readInt bs=case B.readInt bs of{Just(n,_)->n;_->error$"readInt error : bs = "++show bs;}
