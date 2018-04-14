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
main = runGCJ $ do
    [r,c,h,v] <- map read.words <$> getLine
    css <- replicateM r getLine
    putStrLn . bool "IMPOSSIBLE" "POSSIBLE"$ solve r c h v css

solve :: Int -> Int -> Int -> Int -> [String] -> Bool
solve r c h v css
    | a == 0 = True
    | reminder > 0 = False
    | Just (hRes, rest) <- hCut c h v q css = go h hRes rest
    | otherwise = False
  where
    !a = length . filter isChoco $ concat css
    !(q, reminder) = a `divMod` ((h + 1) * (v + 1))
    go 0 _ rest = all (not.isChoco) $ concat rest
    go _ _ [] = False
    go hRest hRes rest = case hCut c h v q rest of
        Just (hRes', rest')
          | res<-merge hRes hRes', length res == length hRes -> go (hRest - 1) res rest'
          | otherwise -> False
        Nothing       -> False

merge ((lx,rx):(lx',rx'):xs) ((ly,ry):(ly',ry'):ys)
   | rx < ly', ry < lx' = (min lx ly,max rx ry):merge((lx',rx'):xs)((ly',ry'):ys)
   | otherwise = []
merge [(lx,rx)] [(ly,ry)] = [(min lx ry,max rx ry)]

hCut :: Int -> Int -> Int -> Int -> [String] -> Maybe ([(Int, Int)], [String])
hCut c h v q css = go 0 (replicate c 0) css
  where
    go !acc freq cscss@(cs:css)
        | acc == (v + 1) * q = case vCut v q freq of
            Just res -> Just (res, cscss)
            Nothing  -> Nothing
        | acc < (v + 1) * q = go (acc + countH cs) (zipWith (+) freq $ chocoCountH cs) css
        | otherwise = Nothing
    go !acc freq []
        | acc == (v + 1) * q = case vCut v q freq of
            Just res -> Just (res, [])
            Nothing  -> Nothing
        | otherwise = Nothing

nothing :: Int
nothing = -1

vCut :: Int -> Int -> [Int] -> Maybe [(Int, Int)]
vCut v q xs = go (v+1) 0 nothing [] $ zip[0..]xs
  where
    !len = length xs
    go :: Int -> Int -> Int -> [(Int,Int)] -> [(Int,Int)] -> Maybe [(Int,Int)]
    go rest !acc l rev ((i,x):ixs)
        | acc == q = go (rest - 1) 0 nothing ((l,i-1):rev) $ (i,x):ixs
        | acc > q = Nothing
        | l == nothing, x > 0 = go rest x i rev ixs
        | otherwise = go rest (acc+x) l rev ixs
    go rest acc l rev []
        | rest == 1, acc == q = Just $ reverse $ (l,len - 1):rev
        | rest == 0, acc == 0, l == nothing = Just $ reverse rev
        | otherwise = Nothing

chocoCountH :: String -> [Int]
chocoCountH = map (fromEnum.isChoco)

countH :: String -> Int
countH = sum . chocoCountH

isChoco :: Char -> Bool
isChoco c = c == '@'

-------------------------------------------------------------------------------

type Parser a = StateT B.ByteString Maybe a

parseInt :: Parser Int
parseInt = coerce $ B.readInt . B.dropWhile isSpace

parseInt2 :: Parser (Int, Int)
parseInt2 = (,) <$> parseInt <*> parseInt

rep, rev :: Applicative f => Int -> (Int -> f ()) -> f ()
rep n f=F.traverse_ f[0..n-1]
rev n f=F.traverse_(f.negate)[1-n..0]
for :: Applicative f => Int -> Int -> (Int -> f ()) -> f ()
for a b f=F.traverse_ f[a..b]
{-# INLINE rep #-}
{-# INLINE rev #-}
{-# INLINE for #-}

modifyArray :: (MArray a e m, Ix i) => a i e -> i -> (e -> e) -> m ()
modifyArray a i f=readArray a i>>=writeArray a i.f
{-# INLINE modifyArray #-}
unsafeModify :: (MArray a e m, Ix i) => a i e -> Int -> (e -> e) -> m ()
unsafeModify a i f=unsafeRead a i>>=unsafeWrite a i.f
{-# INLINE unsafeModify #-}

runGCJ :: IO () -> IO ()
runGCJ main_ = do
    !t <- readLn :: IO Int
    F.for_ [1..t] $ \i -> do
        putStr $ "Case #" ++ shows i ": "
        main_
