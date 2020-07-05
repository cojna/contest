{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns, BinaryLiterals, CPP, DerivingStrategies  #-}
{-# LANGUAGE DerivingVia, FlexibleContexts, FlexibleInstances       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, KindSignatures, LambdaCase #-}
{-# LANGUAGE MagicHash, MultiParamTypeClasses, MultiWayIf           #-}
{-# LANGUAGE NumericUnderscores, OverloadedStrings, PatternSynonyms #-}
{-# LANGUAGE RankNTypes, RecordWildCards, ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving, TupleSections, TypeApplications    #-}
{-# LANGUAGE TypeFamilies, TypeInType, UnboxedTuples, ViewPatterns  #-}

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.Reader
import           Control.Monad.ST
import           Control.Monad.State.Strict
import           Data.Bifunctor
import           Data.Bool
import qualified Data.ByteString               as B
import qualified Data.ByteString.Builder       as B
import qualified Data.ByteString.Char8         as C
import qualified Data.ByteString.Internal      as B
import qualified Data.ByteString.Unsafe        as B
import           Data.Char
import qualified Data.Foldable                 as F
import           Data.Function
import           Data.Functor.Identity
import qualified Data.IntMap.Strict            as IM
import qualified Data.IntSet                   as IS
import qualified Data.List                     as L
import qualified Data.Map.Strict               as M
import           Data.Monoid
import           Data.Ord
import           Data.Primitive
import           Data.Proxy
import           Data.Ratio
import           Data.Semigroup
import qualified Data.Set                      as S
import           Data.Tuple
import qualified Data.Vector                   as V
import qualified Data.Vector.Generic           as G
import qualified Data.Vector.Generic.Mutable   as GM
import qualified Data.Vector.Mutable           as VM
import qualified Data.Vector.Primitive         as P
import qualified Data.Vector.Primitive.Mutable as PM
import qualified Data.Vector.Unboxed           as U
import qualified Data.Vector.Unboxed.Mutable   as UM
import           Debug.Trace
import           Foreign                       hiding (void)
import           GHC.Exts
import           GHC.TypeLits
import qualified System.IO                     as IO
import           Unsafe.Coerce

#define MOD 1000000007

main :: IO ()
main = do
    [n, q] <- map read.words <$> getLine :: IO [Int]
    qs <- U.unfoldrN q (runParser $ (,,)<$>int<*>int<*>int) <$> C.getContents
    B.hPutBuilder IO.stdout . U.foldr ((<>) . bool "No\n" "Yes\n") mempty
        $ solve n q qs

solve :: Int -> Int -> U.Vector (Int, Int, Int) -> U.Vector Bool
solve _ q qs = runST $ do
    uf <- newUnionFind 100100
    fmap (U.map snd . U.filter ((== 1) . fst)) . U.forM qs $ \case
        (0, a, b) -> uniteUF uf a b >> return (0 :: Int, False)
        (1, a, b) -> (,) 1 <$!> equivUF uf a b

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
-- Data.UnionFind
-------------------------------------------------------------------------------
newtype UnionFind s = UF{internalUF :: UM.MVector s Int}
newUnionFind :: PrimMonad m => Int -> m (UnionFind (PrimState m))
newUnionFind n = UF <$> UM.replicate n (-1)
{-# INLINE newUnionFind #-}
freezeUnionFind :: PrimMonad m => UnionFind (PrimState m) -> m (U.Vector Int)
freezeUnionFind = U.unsafeFreeze . internalUF
findUF :: PrimMonad m => UnionFind (PrimState m) -> Int -> m Int
findUF uf x = go x return where { go !x k = do { px <- UM.unsafeRead (internalUF uf) x; if px < 0 then k x else go px $ \ ppx -> do { UM.unsafeWrite (internalUF uf) x ppx; k ppx}}}
{-# INLINE findUF #-}
sizeUF :: PrimMonad m => UnionFind (PrimState m) -> Int -> m Int
sizeUF uf = fix $ \ loop x -> do { px <- UM.unsafeRead (internalUF uf) x; if px < 0 then return $! negate px else loop px}
{-# INLINE sizeUF #-}
uniteUF :: PrimMonad m => UnionFind (PrimState m) -> Int -> Int -> m Bool
uniteUF uf x y = do { px <- findUF uf x; py <- findUF uf y; if px == py then return False else do { rx <- UM.unsafeRead (internalUF uf) px; ry <- UM.unsafeRead (internalUF uf) py; if rx < ry then do { UM.unsafeModify (internalUF uf) (+ ry) px; UM.unsafeWrite (internalUF uf) py px} else do { UM.unsafeModify (internalUF uf) (+ rx) py; UM.unsafeWrite (internalUF uf) px py}; return True}}
{-# INLINE uniteUF #-}
equivUF :: PrimMonad m => UnionFind (PrimState m) -> Int -> Int -> m Bool
equivUF uf x y = (==) `liftM` findUF uf x `ap` findUF uf y
{-# INLINE equivUF #-}
countGroupUF :: PrimMonad m => UnionFind (PrimState m) -> m Int
countGroupUF uf = U.length . U.filter (< 0) <$> freezeUnionFind uf
{-# INLINE countGroupUF #-}
