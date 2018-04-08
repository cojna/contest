{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns #-}

import           Control.Monad
import qualified Data.ByteString.Char8 as B
import           Data.Char
import qualified Data.List             as L
import           Debug.Trace

main :: IO ()
main = runGCJ $ do
    a <- readLn :: IO Double
    mapM_ (putStrLn.unwords.map show)$ solve a

solve :: Double -> [[Double]]
solve a =
    [ [0.5 * cos theta, 0.5 * sin theta, 0.0]
    , [-0.5 * sin theta, 0.5 * cos theta, 0.0]
    , [0.0, 0.0, 0.5]
    ]
  where
    theta = asin (a / sqrt 2.0) - pi / 4

runGCJ :: IO () -> IO ()
runGCJ main_ = do
    t <- readLn
    forM_ [1..t] $ \i -> do
        putStr $ "Case #" ++ shows i ":\n"
        main_
