{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns #-}

import           Control.Applicative
import qualified Data.ByteString.Char8 as B
import           Data.Char

main :: IO ()
main = do
    [_, t] <- map read.words <$> getLine :: IO [Int]
    (xs, ys) <- B.span (/= '.') . B.filter (not.isSpace) <$> B.getLine
    putStr $ solve t xs . tail $ B.unpack ys

solve :: Int -> B.ByteString -> String -> String
solve _ xs (y:ys) | '4' < y = plusOne xs
solve t xs ('4':ys)
    | null rs || head rs < '4' || t <= lf = B.unpack xs ++ '.':solve' t 1 'x' ys
    | otherwise = plusOne xs
  where
    (fs, rs) = span ('4'==) ys
    lf = length fs + 1

solve t xs (y:ys) = B.unpack xs ++ '.':solve' t 0 y ys

solve' = go
  where
    go 0 0 z zs = z:zs
    go 0 f p zs = [p | p/='x'] ++ replicate f '4' ++ zs
    go !t 0 p (z:zs)
      | '4' < z = [succ p]
      | z == '4' = go t 1 p zs
      | otherwise = p : go t 0 z zs
    go !t !f p (z:zs)
      | '4' < z, t >= f + 1 , p /= 'x' = [succ p]
      | '4' < z = [p | p/='x'] ++ replicate (max 0 $ f-t) '4' ++ "5"
      | z == '4' = go t (f + 1) p zs
      | otherwise = [p | p/='x'] ++ replicate f '4' ++ go t 0 z zs
    go _ f p "" | f > 0 = p : replicate f '4'
    go _ _ p "" = [p]
    go _ _ _ "" = ""

plusOne :: B.ByteString -> String
plusOne bs
  | Just (x, _) <- B.readInteger bs = show $ x + 1
  | otherwise = error "readInteger"
