{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns, CPP, FlexibleContexts, FlexibleInstances       #-}
{-# LANGUAGE KindSignatures, LambdaCase, MagicHash, MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf, OverloadedStrings, RecordWildCards               #-}
{-# LANGUAGE ScopedTypeVariables, TupleSections, TypeApplications         #-}
{-# LANGUAGE TypeFamilies, ViewPatterns                                   #-}

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
import           System.Environment
import qualified System.IO                   as IO
import           System.Process
import           Unsafe.Coerce

main :: IO ()
main = withGCJ $ do
    [x, y] <- map read.words <$> getLine :: IO [Int]
    case solve x y of
        Just cs -> putStrLn cs
        Nothing -> putStrLn "IMPOSSIBLE"

eval :: String -> (Int, Int)
eval cs = go 0 0 1 cs
  where
    go !x !y d (c:cs) = case c of
        'N' -> go x (y + d) (2 * d) cs
        'E' -> go (x + d) y (2 * d) cs
        'W' -> go (x - d) y (2 * d) cs
        'S' -> go x (y - d) (2 * d) cs
        _ -> undefined
    go x y _ [] = (x, y)

solve :: Int -> Int -> Maybe String
solve gx gy = go id 0 0 1
  where
    moveE res x y d = go (res . ('E':)) (x + d) y (2 * d)
    moveW res x y d = go (res . ('W':)) (x - d) y (2 * d)
    moveN res x y d = go (res . ('N':)) x (y + d) (2 * d)
    moveS res x y d = go (res . ('S':)) x (y - d) (2 * d)
    go res !x !y !d
--        | traceShow (x, y, d, lsbX, lsbY,res"") False = undefined
        | gx == x && gy == y = Just $ res ""
        | otherwise =  case (lsbX == d, lsbY == d) of
            (True, True) -> Nothing
            (False, False) -> Nothing
            (True, False)
                | gx == x + d, gy == y -> moveE res x y d
                | gx == x - d, gy == y -> moveW res x y d
                | lsbY == 2 * d -> if lsb (abs $ gx - (x + d)) == 2 * d
                                   then moveW res x y d
                                   else moveE res x y d
                | lsb (abs $ gx - (x + d)) == 2 * d -> moveE res x y d
                | lsb (abs $ gx - (x - d)) == 2 * d -> moveW res x y d
                | gx == x + d -> moveE res x y d
                | gx == x - d -> moveW res x y d
            (False, True)
                | gx == x, gy == y + d -> moveN res x y d
                | gx == x, gy == y - d -> moveS res x y d
                | lsbX == 2 * d -> if lsb (abs $ gy - (y + d)) == 2 * d
                                   then moveS res x y d
                                   else moveN res x y d
                | lsb (abs $ gy - (y + d)) == 2 * d -> moveN res x y d
                | lsb (abs $ gy - (y - d)) == 2 * d -> moveS res x y d
                | gy == y + d -> moveN res x y d
                | gy == y - d -> moveS res x y d
      where
        dx = abs $ gx - x
        dy = abs $ gy - y
        lsbX = lsb (abs dx)
        lsbY = lsb (abs dy)

lsb :: Int -> Int
lsb 0 = 0
lsb x = unsafeShiftL 1 (countTrailingZeros x)


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
-------------------------------------------------------------------------------
-- Control.GCJ
-------------------------------------------------------------------------------
formatGCJ :: Int -> String
formatGCJ i = "Case #" <> shows i ": "

withGCJ :: IO () -> IO ()
withGCJ f = getArgs >>= \case
    ["--debug"] -> f
    [] -> do
        t <- readLn
        mapM_ ((*> f) . putStr . formatGCJ) [1..t]
    args -> error $ show args