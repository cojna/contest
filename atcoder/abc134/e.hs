{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
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
import qualified Data.ByteString.Builder     as B
import qualified Data.ByteString.Char8       as C
import qualified Data.ByteString.Internal    as B
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
import qualified Data.Vector.Mutable         as VM
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM
import           Debug.Trace
import           Foreign                     hiding (void)
import           GHC.Exts
import qualified System.IO                   as IO
import           Unsafe.Coerce

main :: IO ()
main = do
    !n <- readLn :: IO Int
    xs <- U.unfoldr parseInt <$> C.getContents
    print $ solve n xs

solve :: Int -> U.Vector Int -> Int
solve n xs = sizeMIS $ U.foldl' step emptyMIS xs
  where
    step mis x = case lookupLTMIS x mis of
        Just y -> insertMIS x $ deleteMIS y mis
        Nothing -> insertMIS x mis

-------------------------------------------------------------------------------
newtype MultiIntSet = MIS {unMIS :: IM.IntMap Int}

emptyMIS :: MultiIntSet
emptyMIS =  MIS IM.empty

isEmptyMIS :: MultiIntSet -> Bool
isEmptyMIS = IM.null . unMIS

sizeMIS :: MultiIntSet -> Int
sizeMIS = IM.foldl' (+) 0 . unMIS

memberMIS :: Int -> MultiIntSet -> Bool
memberMIS x = IM.member x . unMIS

lookupLTMIS :: Int -> MultiIntSet -> Maybe Int
lookupLTMIS x = fmap fst . IM.lookupLT x . unMIS

lookupGTMIS :: Int -> MultiIntSet -> Maybe Int
lookupGTMIS x = fmap fst . IM.lookupGT x . unMIS

lookupLEMIS :: Int -> MultiIntSet -> Maybe Int
lookupLEMIS x = fmap fst . IM.lookupLE x . unMIS

lookupGEMIS :: Int -> MultiIntSet -> Maybe Int
lookupGEMIS x = fmap fst . IM.lookupGE x . unMIS

insertMIS :: Int -> MultiIntSet -> MultiIntSet
insertMIS x = MIS . IM.insertWith (+) x 1 . unMIS

deleteMIS :: Int -> MultiIntSet ->  MultiIntSet
deleteMIS x mset = case IM.lookup x $ unMIS mset of
    Just n | n > 1 -> MIS . IM.insert x (n-1) $ unMIS mset
           | otherwise -> MIS . IM.delete x $ unMIS mset
    Nothing -> mset

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