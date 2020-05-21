{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns, CPP, DeriveFunctor, FlexibleContexts, FlexibleInstances, GADTs  #-}
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
    n <- readLn
    xys <- U.unfoldrN n (runParser $ (,) <$> double <*> double) <$> C.getContents
    print $ solve n xys

double :: Parser Double
double = fmap fromIntegral int

solve :: Int -> U.Vector (Double, Double) -> Double
solve n xys = radiusC $ smallestEnclosingCircle [P x y|(x,y)<-U.toList xys]

solve' :: Int -> U.Vector (Double, Double) -> Double
solve' n xys = res
  where
    low = -1000.0
    high = 1000.0
    ((res, resY), resX) =
        goldenSectionSearchDownward low high $ \cx ->
        goldenSectionSearchDownward low high $ \cy ->
            U.maximum $ U.map (\(x,y) -> norm2 $ (P x y) - (P cx cy)) xys

smallestEnclosingCircle :: [Point] -> Circle
smallestEnclosingCircle ps0 = welzl ps0 []
  where
    welzl [] rs = naiveSmallestEnclosingCircle rs
    welzl _ rs@[_,_,_] = naiveSmallestEnclosingCircle rs
    welzl (p:ps) rs
        | inCircle p c = c
        | otherwise = welzl ps (p:rs)
      where
        c = welzl ps rs

naiveSmallestEnclosingCircle :: [Point] -> Circle
naiveSmallestEnclosingCircle [] = C (P 0.0 0.0) 0.0
naiveSmallestEnclosingCircle [p] = C p 0.0
naiveSmallestEnclosingCircle [p0, p1]
    = C (0.5 *: (p0 + p1)) (norm2 (p1 - p0) * 0.5)
naiveSmallestEnclosingCircle ps = snd $ minimum
    [(r, C c r)|c<-centers, let !r=radius c]
  where
    centers = [0.5 *: (p0 + p1)|p0<-ps, p1<-ps]
        ++ [circumcenter p0 p1 p2
           |p0<-ps, p1<-ps, p2<-ps, abs (area p0 p1 p2) > eps
           ]
    radius c = maximum[norm2 (p - c)|p<-ps]

-- | phi = (1+sqrt 5) / 2
phi :: Double
phi = 1.618033988749895
{-# INLINE phi #-}

-- | resphi = 2-phi = 1/(1+phi)
resphi :: Double
resphi = 0.3819660112501051
{-# INLINE resphi #-}


-- | mid1 (mid1 low high) high = mid2 low high
mid1 :: Double -> Double -> Double
mid1 low high = (low * phi + high) * resphi
{-# INLINE mid1 #-}

-- | mid2 low (mid2 low high) = mid1 low high
mid2 :: Double -> Double -> Double
mid2 low high = (low + high * phi) * resphi
{-# INLINE mid2 #-}

epsGS :: Double
epsGS = 1e-12
{-# INLINE epsGS #-}

goldenSectionSearchDownward
    :: (Ord a) => Double -> Double -> (Double -> a) -> (a, Double)
goldenSectionSearchDownward low high f = go 72 low x1 x2 high (f x1) (f x2)
   where
     !x1 = mid1 low high
     !x2 = mid2 low high
     go !n !x0 !x1 !x2 !x3 !fx1 !fx2
       | n == 0 || abs (x3 - x0) < epsGS = let !k = (x0 + x3) * 0.5
                                               !fk = f k
                                           in (fk, k)
       | fx1 < fx2 = let !x = mid1 x0 x2  -- mid2 x0 x2 == x1
                     in go (n - 1) x0 x x1 x2 (f x) fx1
       | otherwise = let !x = mid2 x1 x3  -- mid1 x1 x3 == x2
                     in go (n - 1) x1 x2 x x3 fx2 (f x)

goldenSectionSearchUpward
    :: (Ord a) => Double -> Double -> (Double -> a) -> (a, Double)
goldenSectionSearchUpward low high f = go 72 low x1 x2 high (f x1) (f x2)
   where
     !x1 = mid1 low high
     !x2 = mid2 low high
     go !n !x0 !x1 !x2 !x3 !fx1 !fx2
       | n == 0 || abs (x3 - x0) < epsGS = let !k = (x0 + x3) * 0.5
                                               !fk = f k
                                           in (fk, k)
       | fx1 > fx2 = let !x = mid1 x0 x2  -- mid2 x0 x2 == x1
                     in go (n - 1) x0 x x1 x2 (f x) fx1
       | otherwise = let !x = mid2 x1 x3  -- mid1 x1 x3 == x2
                     in go (n - 1) x1 x2 x x3 fx2 (f x)

data Circle = C
    { centerC :: !Point  -- ^ center
    , radiusC :: !Double -- ^ radius
    } deriving (Eq, Ord, Show)

inCircle :: Point -> Circle -> Bool
inCircle (P x0 y0) (C (P x y) r) = dx * dx + dy * dy <= (r + eps) * (r + eps)
  where
    !dx = x - x0
    !dy = y - y0

triangleCenter
    :: Double -> Double -> Double
    -> Point -> Point -> Point
    -> Point
triangleCenter wa wb wc a b c
    = recip (wa + wb + wc)
    *: (wa *: a + wb *: b + wc *: c)

incenter :: Point -> Point -> Point -> Point
incenter a b c
    = triangleCenter aa bb cc a b c
  where
    cc = norm2 (a - b)
    aa = norm2 (b - c)
    bb = norm2 (c - a)

centroid :: Point -> Point -> Point -> Point
centroid a b c = recip 3.0 *: (a + b + c)

circumcenter :: Point -> Point -> Point -> Point
circumcenter a b c
    = triangleCenter
        (aa * (bb + cc - aa)) (bb * (cc + aa - bb)) (cc * (aa + bb - cc))
        a b c
  where
    aa = sqrNorm2 (b - c)
    bb = sqrNorm2 (c - a)
    cc = sqrNorm2 (a - b)

orthocenter :: Point -> Point -> Point -> Point
orthocenter a b c = a + b + c


eps :: Double
eps = 1e-8

data Point = P !Double !Double deriving (Eq, Ord)

instance Show Point where
    show (P x y) = show [x, y]

instance Num Point where
    (P x0 y0) + (P x1 y1) = P (x0 + x1) (y0 + y1)
    (P x0 y0) - (P x1 y1) = P (x0 - x1) (y0 - y1)
    (P x0 y0) * (P x1 y1) = P (x0 * x1 - y0 * y1) (x0 * y1 + x1 * y0)
    negate (P x y) = P (negate x) (negate y)
    abs = id
    signum _ = P 1.0 0.0
    fromInteger n = P (fromInteger n) 0

instance Fractional Point where
    v0@(P x0 y0) / v1@(P x1 y1) = P (dot v0 v1 / rr) (cross v1 v0 / rr)
      where
        !rr = sqrNorm2 v1
    fromRational q = P (fromRational q) 0

infixr 7 *:
(*:) :: Double -> Point -> Point
(*:) !k (P x y) = P (k * x) (k * y)
{-# INLINE (*:) #-}

dot :: Point -> Point -> Double
dot (P x0 y0) (P x1 y1) = x0 * x1 + y0 * y1
{-# INLINE dot #-}

cross :: Point -> Point -> Double
cross (P x0 y0) (P x1 y1) = x0 * y1 - y0 * x1
{-# INLINE cross #-}

conjugate :: Point -> Point
conjugate (P x y) = P x (-y)
{-# INLINE conjugate #-}

area :: Point -> Point -> Point -> Double
area o u v = (u - o) `cross` (v - o)
{-# INLINE area #-}

compareCCW :: Point -> Point -> Point -> Ordering
compareCCW o = \u v -> compare 0.0 (area o u v)
{-# INLINE compareCCW #-}

compareCW :: Point -> Point -> Point -> Ordering
compareCW o = flip (compareCCW o)
{-# INLINE compareCW #-}

degToRad :: Double -> Double
degToRad deg = deg / 180.0 * pi
{-# INLINE degToRad #-}

radToDeg :: Double -> Double
radToDeg rad = rad / pi * 180.0
{-# INLINE radToDeg #-}

rotateRad :: Double -> Point -> Point
rotateRad a v = v * P (cos a) (sin a)
{-# INLINE rotateRad #-}

rotateDeg :: Double -> Point -> Point
rotateDeg a v = v * P (cos $ degToRad a) (sin $ degToRad a)
{-# INLINE rotateDeg #-}

norm1 :: Point -> Double
norm1 (P x y) = abs x + abs y
{-# INLINE norm1 #-}

norm2 :: Point -> Double
norm2 = sqrt . sqrNorm2
{-# INLINE norm2 #-}

sqrNorm2 :: Point -> Double
sqrNorm2 v = v `dot` v
{-# INLINE sqrNorm2 #-}

normSup :: Point -> Double
normSup (P x y) = abs x `max` abs y
{-# INLINE normSup #-}

normalize :: Point -> Point
normalize v = recip (norm2 v) *: v
{-# INLINE normalize #-}


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
