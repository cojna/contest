{-# OPTIONS_GHC -O2 -funbox-strict-fields #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Control.Monad.State.Strict
import           Data.Bool
import qualified Data.ByteString.Char8       as B
import qualified Data.ByteString.Unsafe      as B
import           Data.Char
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

import           System.IO

main :: IO ()
main = do
    n <- readLn :: IO Int
    solve n

(x,y) # (z, w) = (x ++ z, y ++ w)

solve :: Int -> IO ()
solve n = do
    res0 <- queries [(s1, s2) | s1<-["#", "."], s2 <- ["#", "."]]
    case res0 of
        Nothing -> return ()
        Just cands@(cand:_) -> do
            let goL q = do
                res <- queryFirst $ map (#q) cands
                case res of
                    Nothing -> return ()
                    Just Nothing -> undefined
                    Just (Just l) -> goL l
            let goR q = do
                res <- queryFirst $ map (q#) cands
                case res of
                    Nothing -> return ()
                    Just Nothing -> goL q
                    Just (Just r) -> goR r

            goR cand

type Request = (String, String)
type Response = Maybe Bool

queryFirst :: [Request] -> IO (Maybe (Maybe Request))
queryFirst = go
  where
    go (q:qs) = do
      res <- query q
      case res of
          Just True -> return $ Just (Just q)
          Just False -> go qs
          Nothing -> return Nothing
    go [] = return $ Just Nothing

queries :: [Request] -> IO (Maybe [Request])
queries = go []
  where
    go acc (q:qs) = do
      res <- query q
      case res of
          Just True -> go (q:acc) qs
          Just False -> go acc qs
          Nothing -> return Nothing
    go acc [] = return $ Just $ reverse acc

query :: Request -> IO Response
query (s1, s2) = do
    putStrLn s1
    putStrLn s2
    hFlush stdout
    res <- getLine
    case res of
        "T" -> return $ Just True
        "F" -> return $ Just False
        "end" -> return $ Nothing
