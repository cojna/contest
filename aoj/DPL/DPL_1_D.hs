{-# OPTIONS_GHC -O2 -funbox-strict-fields #-}

import           Control.Applicative
import qualified Data.ByteString.Char8 as B
import           Data.Char
import qualified Data.IntSet           as IS
import qualified Data.Vector.Unboxed   as U

main :: IO ()
main = do
    n <- readLn :: IO Int
    xs <- U.unfoldrN n (B.readInt.B.dropWhile isSpace) <$> B.getContents
    print $ solve xs

solve :: U.Vector Int -> Int
solve xs = IS.size $ U.foldl' step IS.empty xs
  where
    step s x = case IS.lookupGE x s of
        Just y | x < y     -> IS.insert x $ IS.delete y s
               | otherwise -> s
        Nothing            -> IS.insert x s

