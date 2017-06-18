{-# LANGUAGE BangPatterns #-}
import           Control.Applicative
import           Data.List

main = do
  _ <- getLine
  css <- map (map read.words).lines <$> getContents
  let res = solve css
  case res of
      Just (len, ops) -> do
          print len
          putStr.unlines.map show $ ops
      Nothing -> print $ - 1

solve :: [[Int]] -> Maybe (Int, [Op])
solve xss | not $ isValid xss = Nothing
solve xss = Just $ solve' xss `min` fmap (map trans) (solve' (transpose xss))
solve' xss = (length $ rows ++ cols, map Row rows ++ map Col cols)
  where
    rows = concatMap(uncurry$flip replicate).filter((>0).snd).zip[1..] $ map minimum xss
    cols = concatMap(uncurry$flip replicate).filter((>0).snd).zip[1..] $ map minimum.transpose $ map step xss

isValid :: [[Int]] -> Bool
isValid xss = all(0==).concat.map step.transpose $ map step xss


step xs = map (subtract m) xs
  where
    !m = minimum xs

data Op = Row Int | Col Int deriving (Eq, Ord)

instance Show Op where
    show (Row x) = "row " ++ show x
    show (Col x) = "col " ++ show x

trans (Row x) = Col x
trans (Col x) = Row x
