import Data.Complex

main :: IO ()
main = do
    [ax, ay, bx, by] <- map read.words <$> getLine
    _n <- getLine
    es <- map (toC . map read . words) . lines <$> getContents
    print $ solve (Seg (ax :+ ay) (bx :+ by)) es

solve :: Seg Double -> [Complex Double] -> Int
solve seg0 ees@(e:es)
    = succ . (`div` 2)
    . length
    . filter (hasIntersectSeg seg0)
    $ zipWith Seg ees (es ++ [e])

toC :: [a] -> Complex a
toC [x, y] = x :+ y

dot :: (Num a) => Complex a -> Complex a -> a
dot (x0 :+ y0) (x1 :+ y1) = x0 * x1 + y0 * y1

cross :: (Num a) => Complex a -> Complex a -> a
cross (x0 :+ y0) (x1 :+ y1) = x0 * y1 - y0 * x1

area :: (RealFloat a)
    => Complex a -> Complex a -> Complex a -> a
area o u v = (u - o) `cross` (v - o)

data Seg a = Seg !(Complex a) !(Complex a)
    deriving (Eq, Show)

hasIntersectSeg :: (RealFloat a) => Seg a -> Seg a -> Bool
hasIntersectSeg (Seg p0 p1) (Seg q0 q1)
    = area p0 q0 q1 * area p1 q0 q1 < 0.0
        && area q0 p0 p1 * area q1 p0 p1 < 0.0
