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
    n <- readLn :: IO Int
    cs <- U.unfoldrN n (runParser char) <$> C.getLine
    print $ solve n cs

solve :: Int -> U.Vector Char -> Int
solve n cs = sum $ map step [0..n-1]
  where
    step i = case U.unsafeIndex cs i of
        'R' -> freqL 'G' i * freqR 'B' i + freqL 'B' i * freqR 'G' i - invalid 'R' i
        'G' -> freqL 'R' i * freqR 'B' i + freqL 'B' i * freqR 'R' i - invalid 'G' i
        'B' -> freqL 'G' i * freqR 'R' i + freqL 'R' i * freqR 'G' i - invalid 'B' i

    !cumR = U.scanl' (+) 0 $ U.map (bool 0 1 .(== 'R')) cs
    !cumG = U.scanl' (+) 0 $ U.map (bool 0 1 .(== 'G')) cs
    !cumB = U.scanl' (+) 0 $ U.map (bool 0 1 .(== 'B')) cs

    freqL 'R' i = U.unsafeIndex cumR i
    freqL 'G' i = U.unsafeIndex cumG i
    freqL 'B' i = U.unsafeIndex cumB i

    freqR 'R' i = U.unsafeIndex cumR n - U.unsafeIndex cumR (i + 1)
    freqR 'G' i = U.unsafeIndex cumG n - U.unsafeIndex cumG (i + 1)
    freqR 'B' i = U.unsafeIndex cumB n - U.unsafeIndex cumB (i + 1)


    invalid k i = U.length . U.filter id . U.zipWith p (U.reverse $ U.take i cs) $ U.drop (i + 1) cs
      where
        p x y = x /= y && x /= k && y /= k




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
