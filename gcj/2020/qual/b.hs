{-# LANGUAGE BangPatterns, CPP #-}

import           Control.Monad
import qualified Data.List      as L
import           Data.Semigroup

main :: IO ()
main = runGCJ $ do
    cs <- getLine
    print $ solve cs

data Term = Term !Char !(String -> String)

instance Show Term where
    show (Term _ builder) = builder ""

instance Monoid Term where
    mempty = Term maxBound id
    mappend (Term x xs) (Term y ys) = Term (min x y) (xs . ys)

solve :: String -> Term
solve cs
    = mconcat
    . L.foldl' step (map (\c -> Term c (showChar c)) cs)
    $ reverse ['1'..'9']
  where
    eq c (Term x _) (Term y _) = x >= c && y >= c
    paren c (Term x b)
      | c <= x = Term x $ showChar '(' . b . showChar ')'
      | otherwise = Term x b
    step ts c
        =  L.map (paren c . mconcat)
        $ L.groupBy (eq c) ts


runGCJ :: IO () -> IO ()
#ifdef DEBUG
runGCJ = id
#else
runGCJ action = do
    t <- readLn
    forM_ [1..t] $ \i -> do
        putStr $ "Case #" ++ shows i ": "
        action
#endif
