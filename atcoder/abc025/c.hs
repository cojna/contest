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
    (bs, cs) <- U.splitAt 6 . U.unfoldrN 12 (runParser int) <$> C.getContents
    print $ solve bs cs

type Board = U.Vector Char

isEnd :: Board -> Bool
isEnd = U.notElem '.'

isTurn0 :: Board -> Bool
isTurn0 = odd . U.length . U.elemIndices '.'

isTurn1 :: Board -> Bool
isTurn1 = not . isTurn0

emptyBoard :: Board
emptyBoard = U.replicate 9 '.'

assign :: Board -> Int -> Char -> Board
assign s i c = U.modify (\mv -> UM.write mv i c) s

data Env = Env (U.Vector Int) (U.Vector Int)
    deriving (Show)

ix :: Int -> Int -> Int
ix i j = i * 3 + j
{-# INLINE ix #-}

data Score = Score
    { getScore0 :: !Int
    , getScore1 :: !Int
    } deriving (Eq, Ord)

instance Show Score where
    show (Score x y) = shows x "\n" <> show y

score :: Env -> Board -> Score
score env s = Score (score0 env s) (score1 env s)

score0:: Env -> Board -> Int
score0 (Env bs cs) s = assert (isEnd s) $ resB + resC
  where
    resB = sum [bs U.! ix i j| i<-[0,1],j<-[0,1,2], s U.! ix i j == s U.! ix (i + 1) j]
    resC = sum [cs U.! (2 * i + j)| i<-[0,1,2],j<-[0,1], s U.! ix i j == s U.! ix i (j + 1)]

score1 :: Env -> Board -> Int
score1 (Env bs cs) s = assert (U.notElem '.' s) $ resB + resC
  where
    resB = sum [bs U.! ix i j| i<-[0,1],j<-[0,1,2], s U.! ix i j /= s U.! ix (i + 1) j]
    resC = sum [cs U.! (2 * i + j)| i<-[0,1,2],j<-[0,1], s U.! ix i j /= s U.! ix i (j + 1)]

cmp0 :: Score -> Score -> Ordering
cmp0 (Score x0 _) (Score x1 _) = compare x0 x1

cmp1 :: Score -> Score -> Ordering
cmp1 (Score _ y0) (Score _ y1) = compare y0 y1

solve :: U.Vector Int -> U.Vector Int -> Score
solve bs cs = memoFixMap dfs emptyBoard
  where
    env = Env bs cs
    dfs memo s
        | isEnd s = pure $! score env s
        | isTurn0 s
            = fmap (V.maximumBy cmp0)
            . V.mapM (\i -> dfs memo (assign s i 'o'))
            . U.convert
            $ U.elemIndices '.' s
        | otherwise
            = fmap (V.maximumBy cmp1)
            . V.mapM (\i -> dfs memo (assign s i 'x'))
            . U.convert
            $ U.elemIndices '.' s

memoFixMap
    :: (Ord k)
    => ((k -> State (M.Map k a) a) -> k -> State (M.Map k a) a)
    -> k -> a
memoFixMap f k = flip evalState M.empty $ do
    flip fix k $ \memo x -> do
        gets (M.lookup x) >>= \case
            Just fx -> pure fx
            Nothing -> f memo x >>= \fx ->
                modify' (M.insert x fx) *> pure fx

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
