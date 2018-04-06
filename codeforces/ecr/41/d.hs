{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

import           Control.Applicative
import           Control.Monad
import           Control.Monad.ST.Safe
import           Control.Monad.State.Strict
import           Data.Array.Unboxed
import           Data.Bits
import           Data.Bool
import qualified Data.ByteString.Char8      as B
import           Data.Char
import           Data.Function
import           Data.Int
import qualified Data.IntMap.Strict         as IM
import qualified Data.IntSet                as IS
import qualified Data.List                  as L
import           Data.List.Split
import qualified Data.Map.Strict            as M
import           Data.Monoid
import           Data.Ord
import qualified Data.Set                   as S
import           Data.Tuple
import           Data.Word

main :: IO ()
main = do
    n <- readLn :: IO Int
    ps <- L.unfoldr(runStateT $ mkPoint <$> parseInt <*> parseInt) <$> B.getContents
    putStrLn.bool"NO""YES" $ solve ps

mkPoint :: Int -> Int -> Vec2 Int64
mkPoint x y = V2 (fromIntegral x) (fromIntegral y)

type Parser a = StateT B.ByteString Maybe a

parseInt :: Parser Int
parseInt = StateT $ B.readInt . B.dropWhile isSpace

parseInt2 :: Parser (Int, Int)
parseInt2 = (,) <$> parseInt <*> parseInt

solve :: [Point] -> Bool
solve ps = case convexHull ps of
    [_] -> True
    [_, _] -> True
    [x, y, z] -> or
        [ validate3 (x, y) ps
        , validate3 (y, z) ps
        , validate3 (z, x) ps
        ]
    [x, y, z, w] -> or
        [ validate4 (x, y) (z, w) ps
        , validate4 (x, z) (y, w) ps
        , validate4 (x, w) (y, z) ps
        ]
    _ -> False

validate3 :: Seg -> [Point] -> Bool
validate3 seg ps = case convexHull $ filter (not.flip onLine seg) ps of
    [_]    -> True
    [_, _] -> True
    _      -> False

validate4 :: Seg -> Seg -> [Point] -> Bool
validate4 seg0 seg1 ps = all (\o -> onLine o seg0 || onLine o seg1) ps

data Vec2 a = V2 !a !a deriving (Eq, Ord)

instance Show a => Show (Vec2 a) where
    show (V2 x y) = show [x, y]

instance Functor Vec2 where
    fmap f (V2 x y) = V2 (f x) (f y)

instance (Num a) => Num (Vec2 a) where
    (V2 x0 y0) + (V2 x1 y1) = V2 (x0 + x1) (y0 + y1)
    (V2 x0 y0) - (V2 x1 y1) = V2 (x0 - x1) (y0 - y1)
    (V2 x0 y0) * (V2 x1 y1) = V2 (x0 * x1 - y0 * y1) (x0 * y1 + x1 * y0)
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger n = V2 (fromInteger n) 0

instance (Fractional a) => Fractional (Vec2 a) where
    v0@(V2 x0 y0) / v1@(V2 x1 y1) = recip rr *: V2 x y
      where
        !rr = sqrMagnitude v1
        !x = v0 `dot` v1
        !y = v1 `cross` v0
    fromRational q = V2 (fromRational q) 0

infixr 7 *:
(*:) :: Num a => a -> Vec2 a -> Vec2 a
(*:) k = fmap (k*)
{-# INLINE (*:) #-}

dot :: Num a => Vec2 a -> Vec2 a -> a
dot (V2 x0 y0) (V2 x1 y1) = x0 * x1 + y0 * y1
{-# INLINE dot #-}

cross :: Num a => Vec2 a -> Vec2 a -> a
cross (V2 x0 y0) (V2 x1 y1) = x0 * y1 - y0 * x1
{-# INLINE cross #-}

magnitude :: Floating a => Vec2 a -> a
magnitude = sqrt . sqrMagnitude
{-# INLINE magnitude #-}

sqrMagnitude :: Num a => Vec2 a -> a
sqrMagnitude v = v `dot` v
{-# INLINE sqrMagnitude #-}

normalize :: Floating a => Vec2 a -> Vec2 a
normalize v = recip (magnitude v) *: v
{-# INLINE normalize #-}

type K = Int64
type Point = Vec2 K

type Seg = (Point, Point)
type Polygon = [Point]

--        v
--       /  <==> area o u v > 0
--  o---u
area :: Point -> Point -> Point -> K
area o u v = (u - o) `cross` (v - o)
{-# INLINE area #-}

--        v
--       /  <==> compCCW o u v == LT
--  o---u
compCCW :: Point -> Point -> Point -> Ordering
compCCW o u v = compare 0 $ area o u v
{-# INLINE compCCW #-}

compCW :: Point -> Point -> Point -> Ordering
compCW o = flip (compCCW o)
{-# INLINE compCW #-}

onSeg :: Point -> Seg -> Bool
onSeg p (u, v) = (u - p) `cross` (v - p) == 0
                 && (u - p) `dot` (v - p) < 0
{-# INLINE onSeg #-}

onLine :: Point -> Seg -> Bool
onLine p (u, v) = area p u v == 0
{-# INLINE onLine #-}

hasIntersect :: Seg -> Seg -> Bool
hasIntersect (p0, p1) (q0, q1) = area p0 q0 q1 * area p1 q0 q1 < 0
                               && area q0 p0 p1 * area q1 p0 p1 < 0
{-# INLINE hasIntersect #-}

type Convex = [Point]

convexHull :: [Point] -> Convex
convexHull [] = []
convexHull ps = reverse . go [leftest] $ L.sortBy (compCCW leftest) sorted
   where
     !(leftest:sorted) = L.sort ps
     go (p:q:conv) (r:rs) = case compCCW q p r of
        LT -> go (r:p:q:conv) rs
        GT -> go (q:conv) (r:rs)
        EQ | (q - p) `dot` (r - p) < 0 -> go (r:q:conv) rs -- q--p--r
           | otherwise                 -> go (p:q:conv) rs -- q--r--p
     go [p] (r:rs) = go [r, p] rs
     go conv [] = conv
     go [] (r:rs) = undefined

