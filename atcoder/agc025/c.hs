{-# LANGUAGE BangPatterns #-}

import           Control.Monad.Trans.State.Strict
import qualified Data.ByteString.Char8            as B
import           Data.Char
import           Data.Coerce
import qualified Data.List                        as L

main :: IO ()
main = do
    n <- readLn
    lrs <- L.unfoldr (runParser parseInt2) <$> B.getContents
    print $ solve n lrs

solve :: Int -> [(Int, Int)] -> Int
solve n lrs = goL 0 0 sortedR sortedL `max` goR 0 0 sortedR sortedL
  where
    (ls, rs) = unzip lrs
    sortedL = L.sortBy (flip compare) ls
    sortedR = L.sort rs
    goR !res !cur (l:ls) (r:rs)
        | cur <= r = goL (res + r - cur) r (l:ls) rs
    goR res cur _ _ = res + abs cur
    goL !res !cur (l:ls) (r:rs)
        | l <= cur = goR (res + cur - l) l ls (r:rs)
    goL res cur _ _ = res + abs cur

type Parser a = StateT B.ByteString Maybe a

runParser :: Parser a -> B.ByteString -> Maybe (a, B.ByteString)
runParser = runStateT

parseInt :: Parser Int
parseInt = coerce $ B.readInt . B.dropWhile isSpace

parseInt2 :: Parser (Int, Int)
parseInt2 = (,) <$> parseInt <*> parseInt

parseInt3 :: Parser (Int, Int, Int)
parseInt3 = (,,) <$> parseInt <*> parseInt <*> parseInt
