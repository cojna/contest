{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Control.Monad.State.Strict
import           Data.Bool
import qualified Data.ByteString             as B
import qualified Data.ByteString.Char8       as C
import qualified Data.ByteString.Unsafe      as B
import           Data.Char
import qualified Data.Foldable               as F
import           Data.Function
import qualified Data.IntMap.Strict          as IM
import qualified Data.IntSet                 as IS
import qualified Data.List                   as L
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
import           GHC.Exts
import           Unsafe.Coerce

main :: IO ()
main = do
    [n, q] <- map read.words <$> getLine :: IO [Int]
    cs <- C.getLine
    qs <- U.unfoldrN q parseChar2 <$> C.getContents
    print $ solve n cs qs

parseChar2 :: Parser (Char, Char)
parseChar2 = runStateT $
    (,) <$> StateT (C.uncons . C.dropWhile isSpace)
        <*> StateT (C.uncons . C.dropWhile isSpace)

solve :: Int -> C.ByteString -> U.Vector (Char, Char) -> Int
solve n cs qs = pred . uncurry (flip (-)) $ U.foldr' step (0, n + 1) qs
  where
    !bs = '#' `C.cons` C.snoc cs '#'
    step (c,'L') (l, r) = (l', r')
      where
        !l' | l == n + 1 = n + 1
            | C.index bs (l + 1) == c = l + 1
            | otherwise = l
        !r' | r == n + 1 = n + 1
            | C.index bs r == c = r + 1
            | otherwise = r
    step (c,'R') (l, r) = (l', r')
      where
        !l' | l == 0 = 0
            | C.index bs l == c = l - 1
            | otherwise = l
        !r' | r == 0 = 0
            | c == C.index bs (r - 1) = r - 1
            | otherwise = r

-------------------------------------------------------------------------------
type Parser a = C.ByteString -> Maybe (a, C.ByteString)

parseInt :: Parser Int
parseInt = C.readInt . C.dropWhile isSpace

parseInt2 :: Parser (Int, Int)
parseInt2 = runStateT $
    (,) <$> StateT (C.readInt . C.dropWhile isSpace)
        <*> StateT (C.readInt . B.unsafeTail)

parseInt3 :: Parser (Int, Int, Int)
parseInt3 = runStateT $
    (,,) <$> StateT (C.readInt . C.dropWhile isSpace)
        <*> StateT (C.readInt . B.unsafeTail)
        <*> StateT (C.readInt . B.unsafeTail)

parseInt4 :: Parser (Int, Int, Int, Int)
parseInt4 = runStateT $
    (,,,) <$> StateT (C.readInt . C.dropWhile isSpace)
        <*> StateT (C.readInt . B.unsafeTail)
        <*> StateT (C.readInt . B.unsafeTail)
        <*> StateT (C.readInt . B.unsafeTail)