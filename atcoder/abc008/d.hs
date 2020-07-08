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
    [h, w] <- map read.words <$> getLine :: IO [Int]
    n <- readLn :: IO Int
    xys <- U.unfoldrN n (runParser $ (,) <$> int <*> int) <$> C.getContents
    print $ solve n h w xys

data S = S
    { xMin :: !Int
    , xMax :: !Int
    , yMin :: !Int
    , yMax :: !Int
    }
    deriving (Eq, Ord, Show)

validateS :: S -> Bool
validateS S{..} = xMin < xMax && yMin < yMax

score :: S -> (Int, Int) -> Int
score s@S{..} (x, y) = sum
    [ x - xMin - 1
    , xMax - x - 1
    , y - yMin - 1 
    , yMax - y - 1
    , 1
    ]

inBoard :: S -> (Int, Int) -> Bool
inBoard S{..} (x, y) = xMin < x && x < xMax && yMin < y && y < yMax

solve :: Int -> Int -> Int -> U.Vector (Int, Int) -> Int
solve n h w ms0 = flip memoFixMap (S 0 (h + 1) 0 (w + 1)) $ \dfs s ->
    let !ms = U.filter (inBoard s) ms0
    in  if U.null ms
        then pure $! 0
        else do
            fmap U.maximum . U.forM ms $ \m -> do
                F.foldl' (+) (score s m) <$!> mapM dfs (divide s m)

divide :: S -> (Int, Int) -> [S]
divide S {..} (x, y) = filter validateS
    [ S x xMax y yMax
    , S x xMax yMin y
    , S xMin x y yMax
    , S xMin x yMin y
    ]

memoFixMap
    :: (Ord k, Show k, Show a)
    => ((k -> State (M.Map k a) a) -> k -> State (M.Map k a) a)
    -> k -> a
memoFixMap f k = flip evalState M.empty $ do
    flip fix k $ \memo x -> do
        gets (M.lookup x) >>= \case
            Just fx -> pure fx
            Nothing -> do
                f memo x >>= \fx ->
                    modify' (M.insert x fx) *> pure fx
{-# INLINE memoFixMap #-}                    

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
