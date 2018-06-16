{-# LANGUAGE BangPatterns #-}

import           Control.Monad.Trans.State.Strict
import qualified Data.ByteString.Char8            as B
import           Data.Char
import           Data.Coerce
import qualified Data.List                        as L

main :: IO ()
main = do
    [_, m] <- map read.words <$> getLine
    xyzs <- L.unfoldr (runParser parseInt3) <$> B.getContents
    print $ solve m xyzs

solve :: Int -> [(Int, Int, Int)] -> Int
solve m xyzs = maximum [sum . take m . L.sortBy (flip compare) $ map (priority sign) xyzs|sign<-signs]
  where
    signs = (,,) <$> [-1, 1] <*> [-1, 1] <*> [-1, 1]
    priority (x0,y0,z0) (x1,y1,z1) = x0 * x1 + y0 * y1 + z0 * z1

type Parser a = StateT B.ByteString Maybe a

runParser :: Parser a -> B.ByteString -> Maybe (a, B.ByteString)
runParser = runStateT

parseInt :: Parser Int
parseInt = coerce $ B.readInt . B.dropWhile isSpace

parseInt2 :: Parser (Int, Int)
parseInt2 = (,) <$> parseInt <*> parseInt

parseInt3 :: Parser (Int, Int, Int)
parseInt3 = (,,) <$> parseInt <*> parseInt <*> parseInt
