{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Control.Monad.State.Strict
import           Data.Bool
import qualified Data.ByteString.Char8       as B
import qualified Data.ByteString.Unsafe      as B
import           Data.Char
import qualified Data.Foldable               as F
import           Data.Function
import qualified Data.IntMap.Strict          as IM
import qualified Data.IntSet                 as IS
import           Data.List
import qualified Data.Map.Strict             as M
import           Data.Monoid
import           Data.Ord
import           Data.Primitive.MutVar
import qualified Data.Set                    as S
import           Data.Tuple
import qualified Data.Vector                 as V
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM
import           Debug.Trace
import           Foreign                     hiding (void)
import           Unsafe.Coerce

main :: IO ()
main = do
    !n <- readLn :: IO Int
    xys <- U.unfoldrN n parseInt2 <$> B.getContents
    case solve xys of
        Nothing -> print (-1)
        Just (ds, ws) -> do
            print $ length ds
            putStrLn . unwords $ map show ds
            putStr $ unlines ws

solve :: U.Vector (Int, Int) -> Maybe ([Int], [String])
solve xys
    | U.any parity xys && U.any (not.parity) xys = Nothing
    | U.any (not.parity) xys = case solve $ U.map (\(x, y) -> (x - 1, y)) xys of
        Just (ds, ws) -> Just (1:ds, map ('R':) ws)
        Nothing -> undefined
    | otherwise = Just (map (2^) [0..31], map (uncurry f) uvs)
  where
    m = 32
    uvs = U.toList $ U.map (\(x, y) -> (x + y, x - y)) xys
    f u v = zipWith dir (bits u) (bits v)
    bits x = map (testBit x') [0..31]
      where
        x' = (x + (2^m - 1)) `div` 2
    dir False False = 'L'
    dir True True = 'R'
    dir False True = 'D'
    dir True False = 'U'

parity :: (Int, Int) -> Bool
parity (x, y) = mod (x + y) 2 == 1

-------------------------------------------------------------------------------
type Parser a = B.ByteString -> Maybe (a, B.ByteString)

parseInt :: Parser Int
parseInt = B.readInt . B.dropWhile isSpace

parseInt2 :: Parser (Int, Int)
parseInt2 = runStateT $
    (,) <$> StateT (B.readInt . B.dropWhile isSpace)
        <*> StateT (B.readInt . B.unsafeTail)

parseInt3 :: Parser (Int, Int, Int)
parseInt3 = runStateT $
    (,,) <$> StateT (B.readInt . B.dropWhile isSpace)
        <*> StateT (B.readInt . B.unsafeTail)
        <*> StateT (B.readInt . B.unsafeTail)

parseInt4 :: Parser (Int, Int, Int, Int)
parseInt4 = runStateT $
    (,,,) <$> StateT (B.readInt . B.dropWhile isSpace)
        <*> StateT (B.readInt . B.unsafeTail)
        <*> StateT (B.readInt . B.unsafeTail)
        <*> StateT (B.readInt . B.unsafeTail)