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
    !s <- readLn
    dabs <- replicateM s $ do
      [d,a,b] <- map read.words <$> getLine
      return $!! (d, a, b)
    let (best, numBest) = solveSet1 dabs
    putStrLn . unwords $ map show [best, numBest]

solveSet1 :: [(Int, Int, Int)] -> (Int, Int)
solveSet1 dabs = last.map (\g -> (head g, length g)).L.group.L.sort $ go dabs
  where
    go ((d,a,b):rest) = [1..max accM accN] ++ go rest
      where
        accM = goM 1 (d + a) Nothing rest
        accN = goN 1 Nothing (d - b) rest
    go [] = []

    goM !acc !m (Just n) ((d,a,b):rest)
        | d + a == m || d - b == n = goM (acc + 1) m (Just n) rest
        | otherwise = acc
    goM !acc !m Nothing ((d,a,b):rest)
        | d + a == m = goM (acc + 1) m Nothing rest
        | otherwise = goM (acc + 1) m (Just (d - b)) rest
    goM !acc _ _ [] = acc

    goN !acc (Just m) n ((d,a,b):rest)
        | d + a == m || d - b == n = goN (acc + 1) (Just m) n rest
        | otherwise = acc
    goN !acc Nothing !n ((d,a,b):rest)
        | d - b == n = goN (acc + 1) Nothing n rest
        | otherwise = goN (acc + 1) (Just (d + a)) n rest
    goN !acc _ _ [] = acc

-------------------------------------------------------------------------------

type Parser a = StateT B.ByteString Maybe a

parseInt :: Parser Int
parseInt = coerce $ B.readInt . B.dropWhile isSpace

parseInt2 :: Parser (Int, Int)
parseInt2 = (,) <$> parseInt <*> parseInt

parseInt3 :: Parser (Int, Int, Int)
parseInt3 = (,,) <$> parseInt <*> parseInt <*> parseInt

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
