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
    xs <- U.unfoldr parseInt <$> C.getLine
    let res = solve n xs
    print $ U.length res
    B.hPutBuilder IO.stdout $ U.foldr (\x b -> B.intDec x <> B.char7 ' ' <> b) mempty res

solve :: Int -> U.Vector Int -> U.Vector Int
solve n xs = U.map fst.U.filter ((==1).snd) $ U.indexed $ U.create $ do
    a <- UM.replicate (n + 1) 0
    flip U.imapM xs $ \i x -> do
        UM.unsafeWrite a (i + 1) x
    U.forM_ (U.iterateN n (subtract 1) n) $ \i -> do
        ai <- UM.unsafeRead a i
        when (ai == 1) $ do
            forM_ (tail $ divs V.! i) $ \d -> do
                UM.unsafeModify a (1-) d
    return a
  where
    !divs = buildDivisors n

buildDivisors :: Int -> V.Vector [Int]
buildDivisors n = V.create $ do
    divisors <- VM.replicate (n + 1) [1]
    U.forM_ (U.generate (n - 1) (+2)) $ \i -> do
        U.forM_ (U.iterateN (quot n i) (+i) i) $ \j -> do
            VM.unsafeModify divisors (i:) j
    return divisors

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