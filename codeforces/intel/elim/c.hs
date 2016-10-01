{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns #-}
import           Control.Applicative
import           Control.Monad
import           Data.Array
import qualified Data.ByteString.Char8 as B
import           Data.Int
import qualified Data.IntMap           as IM


main :: IO ()
main = do
  n <- readLn :: IO Int
  xs <- map (fromIntegral.readInt).B.words <$> B.getLine
  ys <- map (subtract 1.readInt).B.words <$> B.getLine
  putStr.unlines.map show $ solve n xs ys

solve :: Int -> [Int64] -> [Int] -> [Int64]
solve n xs ys = go [] 0 IM.empty IM.empty $ reverse ys
  where
    arr = listArray(0, n-1) xs
    go res acc ls rs (k:ks) = go (acc:res) res' ls' rs' ks
      where
        !res' = max acc s
        !v = arr ! k
        !s = sl + v + sr
        (l, sl) = case IM.lookup (k-1) rs of
            Just (lk, s) -> (lk, s)
            Nothing -> (k, 0)
        (r, sr) = case IM.lookup (k+1) ls of
            Just (rk, s) -> (rk, s)
            Nothing -> (k, 0)
        rs' = IM.insert r (l, s) $ IM.delete (k-1) rs
        ls' = IM.insert l (r, s) $ IM.delete (k+1) ls
    go res _ _ _ [] = res

readInt :: B.ByteString -> Int
readInt bs=case B.readInt bs of{Just(n,_)->n;_->error$"readInt error : bs = "++show bs;}
