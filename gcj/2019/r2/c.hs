{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Control.Monad.State.Strict
import           Data.Bool
import qualified Data.ByteString             as B
import qualified Data.ByteString.Char8       as C
import qualified Data.ByteString.Unsafe      as B
import           Data.Char
import qualified Data.Foldable               as F
import           Data.Function
import qualified Data.IntMap.Strict          as IM
import qualified Data.IntSet                 as IS
import qualified Data.List                   as L
import qualified Data.Map.Strict             as M
import           Data.Monoid
import           Data.Ord
import           Data.Primitive.MutVar
import qualified Data.Set                    as S
import           Data.Tuple
import qualified Data.Vector                 as V
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM
import           Debug.Trace
import           Foreign                     hiding (void)
import           GHC.Exts
import           GHC.Real
import           Unsafe.Coerce

main :: IO ()
main = runGCJ $ do
    n <- readLn
    xys <- replicateM n $ do
        [x, y] <- map read.words <$> getLine
        return (x, y)
    putStrLn . maybe "IMPOSSIBLE" showPair $ solve n xys

showPair :: (Int, Int) -> String
showPair (x, y) = shows x $ ' ' : show y

solve :: Int -> [(Int, Int)] -> Maybe (Int, Int)
solve n cjs
    | lb >= ub = Nothing
    | all validate diffs
    , (p :/ q) <- sternBrocot approx = Just (q, p)
    | otherwise = Nothing
  where
    diffs = zipWith (\(x1, y1) (x0, y0) -> (x1 - x0, y1 - y0)) (tail cjs) cjs

    validate (dx, dy)
        | dx <= 0 = dy > 0
        | dy <= 0 = dx > 0
        | otherwise = True

    !lb = maximum $ (0 :/ 1) : [(-dx) :/ dy|(dx,dy)<-diffs, dy>0]
    !ub = minimum $ (1 :/ 0) : [dx :/ (-dy)|(dx,dy)<-diffs, dy<0]

    approx q
        | ub <= q = GT
        | q <= lb = LT
        | otherwise = EQ

sternBrocot :: (Frac -> Ordering) -> Frac
sternBrocot approx = go (0 :/ 1) (1 :/ 0)
  where
    go !l@(lp :/ lq) !r@(rp :/ rq) = case approx m of
        LT -> go m r
        GT -> go l m
        EQ -> m
      where
        !m@(mp :/ mq) = (lp + rp) :/ (lq + rq)

data Frac = !Int :/ !Int deriving (Eq)

frac x y = case gcd x y of
    g -> (signum y * quot x g) :/ quot (abs y) g

instance Show Frac where
    show (x :/ y) = shows x $ '/' : show y

instance Ord Frac where
    compare (x0 :/ y0) (x1 :/ y1) = compare (x0 * y1) (x1 * y0)

-------------------------------------------------------------------------------

runGCJ :: IO () -> IO ()
#ifdef DEBUG
runGCJ = id
#else
runGCJ main_ = do
    t <- readLn
    forM_ [1..t] $ \i -> do
        putStr $ "Case #" ++ shows i ": "
        main_
#endif