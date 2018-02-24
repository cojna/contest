{-# OPTIONS_GHC -O2 -funbox-strict-fields #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Control.Monad.State.Strict
import           Data.Bool
import qualified Data.ByteString.Char8       as B
import qualified Data.ByteString.Unsafe      as B
import           Data.Char
import           Data.Function
import qualified Data.IntMap.Strict          as IM
import qualified Data.IntSet                 as IS
import           Data.List
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
import           Unsafe.Coerce

main :: IO ()
main = do
    !n <- readLn :: IO Int
    points <- map(\(x,y)->P (fromIntegral x) (fromIntegral y)).U.toList.U.unfoldrN n parseInt2 <$> B.getContents
    mapM_ print $ solve n points

solve :: Int -> [Point] -> [Double]
solve 2 _      = [0.5, 0.5]
solve n points = [maybe 0.0 id $ M.lookup p probs|p<-points]
    where
      convex = convexHull points
      doubleDot x y = fromIntegral $ dot x y
      probs :: M.Map Point Double
      probs = case convex of
          [x, y] -> M.fromList [(x,0.5),(y,0.5)]
          _      -> go M.empty $ convex ++ convex
      go :: M.Map Point Double -> [Point] -> M.Map Point Double
      go res (x:o:y:rest) = go (M.insert o (acos c / pi / 2) res) (o:y:rest)
        where
          c = -doubleDot(x-o)(y-o) / (sqrt(doubleDot(x-o)(x-o)) * sqrt(doubleDot(y-o)(y-o)))
      go res _ = res
        where
          ps = map snd $ M.toList res

-------------------------------------------------------------------------------
type Parser a = B.ByteString -> Maybe (a, B.ByteString)

parseInt :: Parser Int
parseInt = B.readInt . B.dropWhile isSpace

parseInt2 :: Parser (Int, Int)
parseInt2 = runStateT $
    (,) <$> StateT (B.readInt . B.dropWhile isSpace)
    <*> StateT (B.readInt . B.unsafeTail)

data Point = P !Integer !Integer deriving (Eq, Ord)
type Seg = (Point, Point)
type Polygon = [Point]
type Convex = [Point]

instance Show Point where
  show (P x y) = show (x,y)

instance Num Point where
  (P x0 y0) + (P x1 y1) = P (x0 + x1) (y0 + y1)
  (P x0 y0) - (P x1 y1) = P (x0 - x1) (y0 - y1)
  (P x0 y0) * (P x1 y1) = P (x0 * x1 - y0 * y1) (x0 * y1 + x1 * y0)
  negate (P x y) = P (negate x) (negate y)
  abs _ = undefined
  signum _ = undefined
  fromInteger n = P n 0

infixr 7 *:
(*:) :: Integer -> Point -> Point
(*:) !k (P x y) = P (k * x) (k * y)
{-# INLINE (*:) #-}

dot :: Point -> Point -> Integer
dot (P x0 y0) (P x1 y1) = x0 * x1 + y0 * y1
{-# INLINE dot #-}

cross :: Point -> Point -> Integer
cross (P x0 y0) (P x1 y1) = x0 * y1 - y0 * x1
{-# INLINE cross #-}

--        v
--       /  <==> area o u v > 0
--  o---u
area :: Point -> Point -> Point -> Integer
area o u v = (u - o) `cross` (v - o)
{-# INLINE area #-}

--        v
--       /  <==> compCCW o u v == LT
--  o---u
compCCW :: Point -> Point -> Point -> Ordering
compCCW o u v = compare 0 $ area o u v
{-# INLINE compCCW #-}

compCW :: Point -> Point -> Point -> Ordering
compCW o u v = compCCW o v u
{-# INLINE compCW #-}

onSeg :: Point -> Seg -> Bool
onSeg p (q0, q1) = (q0 - p) `cross` (q1 - p) == 0
                 && (q0 - p) `dot` (q1 - p) < 0
{-# INLINE onSeg #-}

segIntersect :: Seg -> Seg -> Bool
segIntersect (p0, p1) (q0, q1) = (area p0 q0 q1) * (area p1 q0 q1) < 0
                               && (area q0 p0 p1) * (area q1 p0 p1) < 0
{-# INLINE segIntersect #-}

convexHull :: [Point] -> Convex
convexHull ps = reverse.go [v0].sortBy (compCCW v0) $ filter (/=v0) ps
   where
     !v0 = minimum ps
     go (p:q:conv) (r:rs) = case compCCW q p r of
        LT -> go (r:p:q:conv) rs
        GT -> go (q:conv) (r:rs)
        EQ | (q-p) `dot` (r-p) < 0 -> go (r:q:conv) rs -- q--p--r
           | otherwise             -> go (p:q:conv) rs -- q--r--p
     go [p] (r:rs) = go [r,p] rs
     go conv [] = conv
