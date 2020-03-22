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
    [n, m, k] <- map read.words <$> getLine :: IO [Int]
    (friend, block) <- U.splitAt m . U.unfoldrN (m + k) (runParser $ (,) <$> int1 <*> int1) <$> C.getContents
    putStrLn.unwords.map show.U.toList $ solve n friend block

solve :: Int -> U.Vector (Int, Int) -> U.Vector (Int, Int) -> U.Vector Int
solve n friend block = U.create $ do
    res <- UM.replicate n 0
    uf <- newUnionFind n
    U.forM_ friend $ \(x, y) -> do
        uniteM uf x y
        UM.unsafeModify res (subtract 1) x
        UM.unsafeModify res (subtract 1) y

    rep n $ \i -> do
        cand <- subtract 1 <$> sizeM uf i
        UM.unsafeModify res (+ cand) i

    U.forM_ block $ \(x, y) -> do
        blocked <- equivM uf x y
        when blocked $ do
            UM.unsafeModify res (subtract 1) x
            UM.unsafeModify res (subtract 1) y

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
-------------------------------------------------------------------------------
-- Data.UnionFind.Vector
-------------------------------------------------------------------------------
newtype UnionFind s = UF{internalUF :: UM.MVector s Int}
runUnionFind :: PrimMonad m => UnionFind (PrimState m) -> m (U.Vector Int)
runUnionFind = U.unsafeFreeze . internalUF
newUnionFind :: PrimMonad m => Int -> m (UnionFind (PrimState m))
newUnionFind n = UF <$> UM.replicate n (-1)
{-# INLINE newUnionFind #-}
findM :: PrimMonad m => UnionFind (PrimState m) -> Int -> m Int
findM uf x = go x return where { go !x k = do { px <- UM.unsafeRead (internalUF uf) x; if px < 0 then k x else go px $ \ ppx -> do { UM.unsafeWrite (internalUF uf) x ppx; k ppx}}}
{-# INLINE findM #-}
sizeM :: PrimMonad m => UnionFind (PrimState m) -> Int -> m Int
sizeM uf = fix $ \ loop x -> do { px <- UM.unsafeRead (internalUF uf) x; if px < 0 then return $! negate px else loop px}
{-# INLINE sizeM #-}
uniteM :: PrimMonad m => UnionFind (PrimState m) -> Int -> Int -> m Bool
uniteM uf x y = do { px <- findM uf x; py <- findM uf y; if px == py then return False else do { rx <- UM.unsafeRead (internalUF uf) px; ry <- UM.unsafeRead (internalUF uf) py; if rx < ry then do { UM.unsafeModify (internalUF uf) (+ ry) px; UM.unsafeWrite (internalUF uf) py px} else do { UM.unsafeModify (internalUF uf) (+ rx) py; UM.unsafeWrite (internalUF uf) px py}; return True}}
{-# INLINE uniteM #-}
equivM :: PrimMonad m => UnionFind (PrimState m) -> Int -> Int -> m Bool
equivM uf x y = (==) `liftM` findM uf x `ap` findM uf y
{-# INLINE equivM #-}
countGroupM :: PrimMonad m => UnionFind (PrimState m) -> m Int
countGroupM uf = U.length . U.filter (< 0) <$> runUnionFind uf
{-# INLINE countGroupM #-}
