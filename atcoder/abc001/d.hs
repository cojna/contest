import qualified Data.List as L

main :: IO ()
main = do
    n <- readLn :: IO Int
    rs <- map read.lines <$> getContents
    putStr.unlines.map show $ solve rs

solve :: [Range] -> [Range]
solve xs = go $ L.sort xs
  where
    go (x:y:xs)
        | hasIntersect x y = go (merge x y:xs)
        | otherwise = x : go (y:xs)
    go xs = xs

newtype Time = Time Int
    deriving (Eq, Ord)

instance Show Time where
    show (Time t) = show02 q ++ show02 r
      where
        (q, r) = quotRem t 100

instance Read Time where
    readsPrec _ cs = case splitAt 4 cs of
        (t@[_, _, _, _], cs') -> [(Time (read t), cs')]
        _                     -> []

show02 :: Int -> String
show02 x
    | x < 10 = "0" ++ show x
    | otherwise = show x

data Range = Range
    { getStartTime :: !Time
    , getEndTime   :: !Time
    } deriving (Eq, Ord)

instance Show Range where
    show (Range start end) = show start ++ "-" ++ show end

instance Read Range where
    readsPrec p cs = do
        (start, '-':cs') <- readsPrec p cs
        (end, cs'') <- readsPrec p cs'
        return $ (Range (down start) (up end), cs'')

down :: Time -> Time
down (Time t)
    | r < 5 = Time (10 * q)
    | otherwise = Time (10 * q + 5)
  where
    (q, r) = quotRem t 10

up :: Time -> Time
up (Time t)
    | r == 0 = (Time t)
    | r <= 5 = Time (10 * q + 5)
    | (h, 5)<-quotRem q 10 = Time ((h + 1) * 100)
    | otherwise = Time (10 * (q + 1))
  where
    (q, r) = quotRem t 10

hasIntersect :: Range -> Range -> Bool
hasIntersect (Range x0 y0) (Range x1 y1)
    = x0 <= y1 && x1 <= y0

merge :: Range -> Range -> Range
merge (Range x0 y0) (Range x1 y1)
    = Range (min x0 x1) (max y0 y1)
