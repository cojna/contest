{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns #-}

import           Control.Applicative
import qualified Data.ByteString.Char8 as B

readInt :: B.ByteString -> Int
readInt bs=case B.readInt bs of{Just(n,_)->n;_->error$"readInt error : bs = "++show bs;}

main :: IO ()
main = do
    [_, m] <- map readInt.B.words <$> B.getLine
    cs <- concat.lines <$> getContents
    case solve m cs of
        Just (x, y) -> do
            putStrLn "YES"
            putStrLn.unwords.map show $ [x+1, y+1]
        Nothing -> putStrLn "NO"

solve :: Int -> String -> Maybe (Int, Int)
solve _ cs | all (=='.') cs = Just (0, 0)
solve m cs = goXY (x0, y0) $ zip [i0+1..] $ drop (i0+1) cs
  where
    i0 :: Int
    !i0 = fst . head . dropWhile ((=='.').snd) $ zip [0..] cs
    (!x0, !y0) = divMod i0 m
    goXY (x, y) ((i, '*'):rest)
        | x == x' = goY (x, y) rest
        | y == y' = goX (x, y) rest
        | otherwise = go (x, y') rest <|> go (x', y) rest
      where
        (x', y') = divMod i m
    goXY (x, y) (_:rest) = goXY (x, y) rest
    goXY (x, y) [] = Just (x, y)

    goX (x, y) ((i, '*'):rest)
        | y == y' = goX (x, y) rest
        | otherwise = go (x', y) rest
      where
        (x', y') = divMod i m
    goX (x, y) (_:rest) = goX (x, y) rest
    goX (x, y) [] = Just (x, y)

    goY (x, y) ((i, '*'):rest)
        | x == x' = goY (x, y) rest
        | otherwise = go (x, y') rest
      where
        (x', y') = divMod i m
    goY (x, y) (_:rest) = goY (x, y) rest
    goY (x, y) [] = Just (x, y)

    go (x, y) rest
        | all (check.fst)$filter((=='*').snd)rest = Just (x, y)
        | otherwise = Nothing
      where
        check i  = x == x' || y == y'
          where
            (x',y') = divMod i m

