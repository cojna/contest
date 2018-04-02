{-# OPTIONS_GHC -O2 #-}
import           Control.Exception
import           Control.Monad
import           Data.Bits
import           Data.List
import           Debug.Trace

main :: IO ()
main = runGCJ $ do
    n <- readLn :: IO Int
    ps <- map read.words <$> getLine
    putStrLn . unwords $ solve ps

solve :: [Int] -> [String]
solve xs = go . sortBy(flip compare) $ zip xs ['A'..]
  where
    go [(x,cx), (y,cy)]   | x == y= replicate x [cx, cy]
    go ((x,cx):(y,cy):xs) = ([cx]:).go $ ins (x - 1, cx) $ (y, cy) : xs
    go _                  = undefined

ins :: (Int, Char) -> [(Int, Char)] -> [(Int, Char)]
ins (0, _) xs = xs
ins (x, c) xs = insertBy (flip compare) (x, c) xs

runGCJ :: IO () -> IO ()
runGCJ main_ = do
    t <- readLn
    forM_ [1..t] $ \i -> do
        putStr $ "Case #" ++ shows i ": "
        main_
