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
    dss <- V.replicateM n $ do
        U.unfoldrN n (runParser int) <$> C.getLine
    q <- readLn :: IO Int
    qs <- U.unfoldrN q (runParser int) <$> C.getContents
    putStr.unlines.map show.U.toList $ solve n dss qs

solve :: Int -> V.Vector (U.Vector Int) -> U.Vector Int -> U.Vector Int
solve n dss qs = U.map (\p -> U.maximum $ U.take (p + 1) scores) qs
  where
    sums :: U.Vector Int
    !sums = U.concat
        . V.toList
        . V.scanl1' (U.zipWith (+))
        $ V.map (U.scanl1' (+)) dss
    ix i j = i * n + j
    !scores = U.create $ do
        res <- UM.replicate ((n+1)*(n+1)) 0
        rep n $ \i -> do
            rep n $ \j -> do
                rep n $ \k -> when (i <= k) $ do
                    rep n $ \l -> when (j <= l) $ do
                        let kl = U.unsafeIndex sums (ix k l)
                        tmp <- UM.replicate 1 kl
                        when (i > 0) $ do
                            let il = U.unsafeIndex sums (ix (i - 1) l)
                            UM.unsafeModify tmp (subtract il) 0
                        when (j > 0) $ do
                            let kj = U.unsafeIndex sums (ix k (j - 1))
                            UM.unsafeModify tmp (subtract kj) 0
                        when (i > 0 && j > 0) $ do
                            let ij = U.unsafeIndex sums (ix (i - 1) (j - 1))
                            UM.unsafeModify tmp (+ij) 0
                        t <- UM.unsafeRead tmp 0
                        UM.unsafeModify res (max t) $ (k - i + 1) * (l - j + 1)
        return res


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
