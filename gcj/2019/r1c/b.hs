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
import qualified System.IO                   as IO
import           System.Process
import           Unsafe.Coerce

main :: IO ()
main = runInteractive $ \hin hout -> do
    let req0 = [1,6..591]
    ps0 <- query hin hout req0
    let (c0, req1) = map succ <$> minGroup ps0
    ps1 <- query hin hout req1
    let (c1, req2) = map succ <$> minGroup ps1
    ps2 <- query hin hout req2
    let (c2, req3@[_]) = map succ <$> minGroup ps2
    ps3 <- query hin hout req3
    let (c4, req4) = map succ <$> minGroup ps3
    let [c3] = filter (`notElem`[c0,c1,c2,c4]) "ABCDE"

    IO.hPutStrLn hout [c0,c1,c2,c3,c4]
    IO.hFlush hout
    "Y" <- IO.hGetLine hin
    return ()

minGroup :: [(Char, Int)] -> (Char, [Int])
minGroup cis = snd.minimum.map f.L.groupBy ((==) `on` fst) $ L.sort cis
  where
    f l@((c,i):_) = (length l, (c, map snd l))

query :: IO.Handle -> IO.Handle -> [Int] -> IO [(Char, Int)]
query hin hout req = forM req $ \i -> do
    IO.hPrint hout i
    IO.hFlush hout
    [c] <- IO.hGetLine hin
    return (c, i)

-------------------------------------------------------------------------------
runInteractive :: (IO.Handle -> IO.Handle -> IO ()) -> IO ()
runInteractive action = do
#ifdef DEBUG
    (Just hout, Just hin, _, _) <- createProcess
        (shell "python3 testing_tool.py 1")
            { std_in = CreatePipe, std_out = CreatePipe}
#else
    let hin = IO.stdin
    let hout = IO.stdout
#endif
    t:_ <- map read.words <$> IO.hGetLine hin :: IO [Int]
    forM_ [1..t] $ \i -> do
        IO.hPutStrLn IO.stderr $ "Case #" ++ shows i ":"
        action hin hout

runGCJ :: IO () -> IO ()
#ifdef DEBUG
runGCJ = id
#else
runGCJ main_ = do
    t <- readLn
    forM_ [1..t] $ \i -> do
        putStr $ "Case #" ++ shows i ": "
        main_
#endif

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