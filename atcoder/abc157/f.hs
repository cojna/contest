{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns, CPP, FlexibleContexts, FlexibleInstances, GADTs  #-}
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
    [n, k] <- map read.words <$> getLine :: IO [Int]
    xycs <- U.unfoldrN n (runParser $ (,,) <$> double <*> double <*> double) <$> C.getContents
    print $ solve n k xycs

double :: Parser Double
double = fromIntegral <$> int


solve :: Int -> Int -> U.Vector (Double, Double, Double) -> Double
solve n k (U.toList -> xycs) = lowerBoundDouble 0.0 1e6 $ \t ->
    let cs = [C x y (t/c) | (x, y, c) <- xycs]
        cands = do
            c0 <- cs
            c1 <- cs
            intersectionPoints c0 c1
    in any ((>= k) . flip score cs) $ map center cs ++ cands

score :: Point -> [Circle] -> Int
score p cs = length $ filter (inCircle p) cs


data Circle = C !Double !Double !Double
    deriving (Eq, Ord, Show)

center :: Circle -> Point
center (C x y _) = P x y

radius :: Circle -> Double
radius (C _ _ r) = r

inCircle :: Point -> Circle -> Bool
inCircle (P x0 y0) (C x y r) = dx * dx + dy * dy <= (r + eps) * (r + eps)
  where
    !dx = x - x0
    !dy = y - y0

eps :: Double
eps = 1e-8

intersectionPoints :: Circle -> Circle -> [Point]
intersectionPoints (C x0 y0 r0) (C x1 y1 r1)
    | abs (r1 - r0) < d, d < r0 + r1
        = [ c + (h / d) *: P (-dy) dx, c + (h / d) *: P dy (-dx)]
    | otherwise = []
  where
    !p0 = P x0 y0
    !p1 = P x1 y1
    !dx = x1 - x0
    !dy = y1 - y0
    !d = sqrt (dx * dx + dy * dy)

    !c = let !k = 0.5 * (d * d + r0 * r0 - r1 * r1) / d
         in recip d *: ((d - k) *: p0 + k *: p1)

    !h = let !s = 0.5 * (r0 + r1 + d)
         in 2.0 * sqrt (s * (s - r0) * (s - r1) * (s - d)) / d


data Point = P !Double !Double deriving (Eq, Ord)

instance Show Point where
    show (P x y) = show [x, y]

instance Num Point where
    (P x0 y0) + (P x1 y1) = P (x0 + x1) (y0 + y1)
    (P x0 y0) - (P x1 y1) = P (x0 - x1) (y0 - y1)
    (P x0 y0) * (P x1 y1) = P (x0 * x1 - y0 * y1) (x0 * y1 + x1 * y0)
    negate (P x y) = P (negate x) (negate y)
    abs = id
    signum = const 1
    fromInteger n = P (fromInteger n) 0

instance Fractional Point where
    v0@(P x0 y0) / v1@(P x1 y1) = P (dot v0 v1 / rr) (cross v1 v0 / rr)
      where
        !rr = sqrNorm2 v1
    fromRational q = P (fromRational q) 0

infixr 7 *:
(*:) :: Double -> Point -> Point
(*:) k (P x y) = P (k * x) (k * y)
{-# INLINE (*:) #-}

dot :: Point -> Point -> Double
dot (P x0 y0) (P x1 y1) = x0 * x1 + y0 * y1
{-# INLINE dot #-}

cross :: Point -> Point -> Double
cross (P x0 y0) (P x1 y1) = x0 * y1 - y0 * x1
{-# INLINE cross #-}

area :: Point -> Point -> Point -> Double
area o u v = (u - o) `cross` (v - o)
{-# INLINE area #-}

compareCCW :: Point -> Point -> Point -> Ordering
compareCCW o = \u v -> comapre 0.0 (area o u v)
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


lowerBoundDouble :: Double -> Double -> (Double -> Bool) -> Double
lowerBoundDouble low high p = go 50 low high
  where
    go !n !low !high
        | n == 0    = high
        | p mid     = go (n - 1) low mid
        | otherwise = go (n - 1) mid high
      where
        mid = (low + high) * 0.5


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
