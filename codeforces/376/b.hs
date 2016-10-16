
import Control.Applicative
import Data.Bool
import qualified Data.ByteString.Char8 as B

readInt :: B.ByteString -> Int
readInt bs=case B.readInt bs of{Just(n,_)->n;_->error$"readInt error : bs = "++show bs;}

main = do
  _ <- getLine
  xs <- map readInt.B.words <$> B.getLine
  putStrLn.bool"NO""YES" $ solve xs

solve :: [Int] -> Bool
solve xs = all isValid $ split xs
  where
    isValid (x:xs) | even x = isValid xs
    isValid (x:y:xs) = isValid $ (y-1):xs
    isValid (_:_) = False
    isValid [] = True

split :: [Int] -> [[Int]]
split xs = case span (0/=) xs of
  (ys, 0:zs) -> ys : split zs
  (ys, []) -> [ys]
  _ -> undefined