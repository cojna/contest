{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns, CPP, FlexibleContexts, FlexibleInstances         #-}
{-# LANGUAGE KindSignatures, LambdaCase, MagicHash, MultiParamTypeClasses   #-}
{-# LANGUAGE MultiWayIf, OverloadedStrings, RecordWildCards                 #-}
{-# LANGUAGE ScopedTypeVariables, TupleSections, TypeFamilies, ViewPatterns #-}

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
import           Data.Functor.Identity
import qualified Data.IntMap.Strict          as IM
import qualified Data.IntSet                 as IS
import qualified Data.List                   as L
import qualified Data.Map.Strict             as M
import           Data.Monoid
import           Data.Ord
import           Data.Primitive.MutVar
import           Data.Ratio
import qualified Data.Set                    as S
import           Data.Tuple
import qualified Data.Vector                 as V
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Mutable         as VM
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM
import           Debug.Trace
import           Foreign                     hiding (void)
import           GHC.Exts
import qualified System.IO                   as IO
import           Unsafe.Coerce

main :: IO ()
main = do
    [n, m] <- map read.words <$> getLine :: IO [Int]
    es <- replicateM m $ do
        [!x, !y] <- map (subtract 1.read).words <$> getLine
        return (x, y)
    print $ solve n m es

solve :: Int -> Int-> [(Int, Int)] -> Int
solve n m es = maximum $ do
    set <- L.subsequences [0..n-1]
    guard $ and [U.unsafeIndex gr (x * n + y) == 1 | x<-set, y<-set]
    return $! length set
  where
    !gr = U.create $ do
        g <- UM.replicate (n * n) (0 :: Int)
        rep n $ \i -> do
            UM.write g (i * n + i) 1
        forM_ es $ \(x, y) -> do
            UM.write g (x * n + y) 1
            UM.write g (y * n + x) 1
        return g


-------------------------------------------------------------------------------
-- Utils
-------------------------------------------------------------------------------
rep :: Monad m => Int -> (Int -> m ()) -> m ()
rep !n = U.forM_ $ U.generate n id
{-# INLINE rep #-}
rev :: Monad m => Int -> (Int -> m ()) -> m ()
rev !n = U.forM_ $ U.iterateN n (subtract 1) (n - 1)
{-# INLINE rev #-}
infixl 8 `shiftRL`, `unsafeShiftRL`
shiftRL = unsafeShiftRL
{-# INLINE shiftRL #-}
unsafeShiftRL (I# x#) (I# i#) = I# (uncheckedIShiftRL# x# i#)
{-# INLINE unsafeShiftRL #-}
type Parser a = StateT C.ByteString Maybe a
runParser :: Parser a -> C.ByteString -> Maybe (a, C.ByteString)
runParser = runStateT
{-# INLINE runParser #-}
int :: Parser Int
int = coerce $ C.readInt . C.dropWhile isSpace
{-# INLINE int #-}
int1 :: Parser Int
int1 = fmap (subtract 1) int
{-# INLINE int1 #-}
char :: Parser Char
char = coerce C.uncons
{-# INLINE char #-}
byte :: Parser Word8
byte = coerce B.uncons
{-# INLINE byte #-}
