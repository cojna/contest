{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}

import           Control.Applicative
import           Control.DeepSeq
import           Control.Exception
import           Control.Monad
import           Control.Monad.ST
import           Control.Monad.Trans.State.Strict
import           Data.Array.Base
import           Data.Array.ST                    (STUArray, runSTUArray)
import           Data.Bits
import           Data.Bool
import qualified Data.ByteString.Char8            as B
import           Data.Char
import           Data.Coerce
import qualified Data.Foldable                    as F
import           Data.Function
import           Data.Int
import qualified Data.IntMap.Strict               as IM
import qualified Data.IntSet                      as IS
import qualified Data.List                        as L
import qualified Data.Map.Strict                  as M
import           Data.Monoid
import           Data.Ord
import qualified Data.Set                         as S
import           Data.STRef
import           Data.Tuple
import           Data.Word
import           Debug.Trace
import           GHC.Arr                          (Array, Ix (..), STArray)
import           GHC.Exts
import           System.Exit
import           System.IO

main :: IO ()
main = do
    t <- readLn :: IO Int
    replicateM_ t $ do
        n <- readLn :: IO Int
        step n (IM.fromList . zip [0..n-1] $ repeat 0) IS.empty
    hFlush stdout

step :: Int -> IM.IntMap Int -> IS.IntSet -> IO ()
step 0 _ _ = return ()
step rest !freq !used = do
    pref <- tail.map read.words <$> getLine
    let unused = filter (`IS.notMember` used) pref
    let freq' = updateFreq pref freq
    case minimumFreq unused freq' of
        Nothing -> do
            print (-1) >> hFlush stdout
            step (rest - 1) freq' used
        Just p -> do
            print p >> hFlush stdout
            step (rest - 1) freq' (IS.insert p used)

updateFreq :: [Int] -> IM.IntMap Int -> IM.IntMap Int
updateFreq pref im = F.foldl' (\m x -> IM.insertWith (+) x 1 m) im pref

minimumFreq :: [Int] -> IM.IntMap Int -> Maybe Int
minimumFreq [] im   = Nothing
minimumFreq pref im = Just . snd $  minimum [(im IM.! p, p)| p <- pref]


