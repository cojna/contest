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
import           System.IO
import           System.Process
import           Unsafe.Coerce

main :: IO ()
main = withGCJ $ do
    n <- readLn :: IO Int
    patterns <- replicateM n $ do
        bs <- C.map convert <$> C.getLine
        return . C.words $! normalize bs
    putStrLn $ maybe "*" C.unpack $ solve patterns

convert :: Char -> Char
convert '*' = ' '
convert c = c

normalize :: C.ByteString -> C.ByteString
normalize bs = ('^' `C.cons` bs) `C.snoc` '$'

type Pattern = [C.ByteString]

solve :: [Pattern] -> Maybe C.ByteString
solve patterns
    | all (exactMatch candidate) patterns = Just . C.tail . C.init $ candidate
    | otherwise = Nothing
  where
    l = L.maximumBy (comparing C.length) $ map head patterns
    ms = map (C.concat . drop 1 . init) patterns
    r = L.maximumBy (comparing C.length) $ map last patterns
    !candidate
        | C.last l == '$' = l
        | C.head r == '^' = r
        | otherwise = l <> C.concat ms <> r

exactMatch :: C.ByteString -> Pattern -> Bool
exactMatch (normalize -> !bs) patterns = go bs patterns
  where
    go !s (p:ps)
        | C.null t = False
        | otherwise = go (C.drop (C.length p) s) ps
      where
        (_, t) = C.breakSubstring p s
    go s [] = True
ecactMatch _ [] = undefined

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
withGCJ :: IO () -> IO ()
withGCJ f = do
    args <- getArgs
    case args of
        ["--debug"] -> f
        [] -> do
            t <- readLn @Int
            forM_ [1..t] $ \i -> do
                putStrLn $ "Case #" ++ shows i ": "
                f
        _ -> error $ show args
