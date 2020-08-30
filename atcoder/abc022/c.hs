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
import qualified Data.ByteString                   as B
import qualified Data.ByteString.Builder           as B
import qualified Data.ByteString.Char8             as C
import qualified Data.ByteString.Internal          as B
import qualified Data.ByteString.Unsafe            as B
import           Data.Char
import qualified Data.Foldable                     as F
import           Data.Function
import           Data.Functor.Identity
import qualified Data.IntMap.Strict                as IM
import qualified Data.IntSet                       as IS
import qualified Data.List                         as L
import qualified Data.Map.Strict                   as M
import           Data.Monoid
import           Data.Ord
import           Data.Primitive
import           Data.Proxy
import           Data.Ratio
import           Data.Semigroup
import qualified Data.Set                          as S
import           Data.Tuple
import qualified Data.Vector                       as V
import qualified Data.Vector.Algorithms.Intro      as Intro
import qualified Data.Vector.Fusion.Stream.Monadic as MS
import qualified Data.Vector.Fusion.Bundle.Monadic as MB
import           Data.Vector.Fusion.Util
import qualified Data.Vector.Generic               as G
import qualified Data.Vector.Generic.Mutable       as GM
import qualified Data.Vector.Mutable               as VM
import qualified Data.Vector.Primitive             as P
import qualified Data.Vector.Primitive.Mutable     as PM
import qualified Data.Vector.Unboxed               as U
import qualified Data.Vector.Unboxed.Mutable       as UM
import           Debug.Trace
import           Foreign                           hiding (void)
import           GHC.Exts
import           GHC.TypeLits
import           System.IO
import           Unsafe.Coerce

#define MOD 1000000007

main :: IO ()
main = do
    [n, m] <- map read.words <$> getLine
    es <- U.unfoldrN m (runParser $ (,,) <$> int1 <*> int1 <*> int) <$> C.getContents
    print . maybe (-1) id $ solve n m es

nothing :: Int
nothing = 0x3f3f3f3f3f3f3f3f

solve :: Int -> Int -> U.Vector (Int, Int, Int) -> Maybe Int
solve n m es = answer $ runST $ do
    d <- UM.replicate (n * n) nothing
    rep n $ \i -> do
        UM.write d (ix i i) 0
    U.forM_ es $ \(src, dst, l) -> do
        UM.write d (ix src dst) l
        UM.write d (ix dst src) l

    MS.foldl' min maxBound . flip MS.mapM (stream 1 n) $ \start -> do
        tmp <- UM.read d (ix 0 start)
        UM.write d (ix 0 start) nothing
        UM.write d (ix start 0) nothing
        dist <- dijkstraDense n start <$> U.freeze d
        UM.write d (ix 0 start) tmp
        UM.write d (ix start 0) tmp
        if tmp == nothing || dist U.! 0 == nothing
        then return nothing
        else return $! tmp + dist U.! 0
 where
    ix i j = i * n + j
    {-# INLINE ix #-}

    answer x
        | x == nothing = Nothing
        | otherwise = Just x

-- | O(V^2)
dijkstraDense :: (U.Unbox w, Num w, Ord w, Bounded w)
    => Int -- ^ n
    -> Int -- ^ src
    -> U.Vector w -- ^ adjacent matrix (n x n)
    -> U.Vector w
dijkstraDense n src gr
    | src >= n || U.length gr /= n * n
        = error "dijkstraDense: Invalid Arguments"
    | otherwise = U.create $ do
        dist <- UM.replicate (n + 1) maxBound
        used <- UM.replicate n False
        UM.write dist src 0
        let nothing = n
        _v <- UM.replicate 1 0
        _dv <- UM.replicate 1 0
        fix $ \loop -> do
            UM.unsafeWrite _v 0 nothing
            UM.unsafeWrite _dv 0 maxBound
            rep n $ \i -> do
                UM.unsafeRead used i >>= \case
                    False -> do
                        di <- UM.unsafeRead dist i
                        dv <- UM.unsafeRead _dv 0
                        when (di < dv) $ do
                            UM.unsafeWrite _v 0 i
                            UM.unsafeWrite _dv 0 di
                    True -> return ()
            v <- UM.unsafeRead _v 0
            when (v /= nothing) $ do
                UM.unsafeWrite used v True
                dv <- UM.unsafeRead _dv 0
                rep n $ \i -> do
                    let di' = dv + U.unsafeIndex gr (v * n + i)
                    UM.unsafeModify dist (min di') i
                loop
        return dist
{-# INLINE dijkstraDense #-}

-------------------------------------------------------------------------------
-- Utils
-------------------------------------------------------------------------------
rep :: (Monad m) => Int -> (Int -> m ()) -> m ()
rep n = flip MS.mapM_ (stream 0 n)
{-# INLINE rep #-}
rev :: (Monad m) => Int -> (Int -> m ()) -> m ()
rev !n = flip MS.mapM_ (streamR 0 n)
{-# INLINE rev #-}
stream :: (Monad m) => Int -> Int -> MS.Stream m Int
stream !l !r = MS.Stream step l where { step x | x < r = return $ MS.Yield x (x + 1) | otherwise = return MS.Done; {-# INLINE [0] step #-}}
{-# INLINE [1] stream #-}
streamR :: (Monad m) => Int -> Int -> MS.Stream m Int
streamR !l !r = MS.Stream step (r - 1) where { step x | x >= l = return $ MS.Yield x (x - 1) | otherwise = return MS.Done; {-# INLINE [0] step #-}}
{-# INLINE [1] streamR #-}
stream' :: (Monad m) => Int -> Int -> Int -> MS.Stream m Int
stream' !l !r !d = MS.Stream step l where { step x | x < r = return $ MS.Yield x (x + d) | otherwise = return MS.Done; {-# INLINE [0] step #-}}
{-# INLINE [1] stream' #-}
infixl 8 `shiftRL`, `unsafeShiftRL`
shiftRL :: Int -> Int -> Int
shiftRL = unsafeShiftRL
{-# INLINE shiftRL #-}
unsafeShiftRL :: Int -> Int -> Int
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
skipSpaces :: Parser ()
skipSpaces = modify' (C.dropWhile isSpace)
{-# INLINE skipSpaces #-}
lowerBoundM :: (Monad m) => Int -> Int -> (Int -> m Bool) -> m Int
lowerBoundM low high p = go low high where { go !low !high | high <= low = return high | otherwise = p mid >>= bool (go (mid + 1) high) (go low mid) where { mid = low + unsafeShiftRL (high - low) 1}}
{-# INLINE lowerBoundM #-}
upperBoundM :: (Monad m) => Int -> Int -> (Int -> m Bool) -> m Int
upperBoundM low high p = do { flg <- p high; if flg then return high else subtract 1 <$!> lowerBoundM low high (fmap not . p)}
{-# INLINE upperBoundM #-}
lowerBound :: Int -> Int -> (Int -> Bool) -> Int
lowerBound low high p = runIdentity (lowerBoundM low high (return . p))
{-# INLINE lowerBound #-}
upperBound :: Int -> Int -> (Int -> Bool) -> Int
upperBound low high p = runIdentity (upperBoundM low high (return . p))
{-# INLINE upperBound #-}
