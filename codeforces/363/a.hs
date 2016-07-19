{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns #-}

import           Control.Applicative
import qualified Data.ByteString.Char8 as B

readInt :: B.ByteString -> Int
readInt bs=case B.readInt bs of{Just(n,_)->n;_->error$"readInt error : bs = "++show bs;}

main :: IO ()
main = do
    _ <- getLine
    lrs <- getLine
    xs <- map readInt.B.words <$> B.getLine
    print . maybe (-1) id $ solve lrs xs

solve :: String -> [Int] -> Maybe Int
solve lrs xs = go0 lrs xs
  where
    go0 ('R':'L':lrs) (x:y:xys) = go (div (y-x) 2) lrs xys
    go0 (_:r:lrs) (_:y:xys) = go0 (r:lrs) (y:xys)
    go0 _ _ = Nothing
    go !res ('R':'L':lrs) (x:y:xys) = go (min res $ div (y-x) 2) lrs xys
    go res (_:r:lrs) (_:y:xys)  = go res (r:lrs) (y:xys)
    go res _ _ = Just res

