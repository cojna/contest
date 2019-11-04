import Data.Fixed

main :: IO ()
main = do
    [deg, dis] <- map read.words <$> getLine
    let w = windPower dis
    putStrLn $ show (dir w (deg * 10)) ++ " " ++ show w

data Direction
    = N
    | NNE
    | NE
    | ENE
    | E
    | ESE
    | SE
    | SSE
    | S
    | SSW
    | SW
    | WSW
    | W
    | WNW
    | NW
    | NNW
    | C
    deriving (Eq, Show)

dir :: WindPower -> Int -> Direction
dir (WindPower 0) _ = C
dir _ deg
    | 34875 <= deg || deg < 1125 = N
    | deg < 3375  = NNE
    | deg < 5625  = NE
    | deg < 7875  = ENE
    | deg < 10125 = E
    | deg < 12375 = ESE
    | deg < 14625 = SE
    | deg < 16875 = SSE
    | deg < 19125 = S
    | deg < 21375 = SSW
    | deg < 23625 = SW
    | deg < 25875 = WSW
    | deg < 28125 = W
    | deg < 30375 = WNW
    | deg < 32625 = NW
    | deg < 34875 = NNW
    | otherwise = undefined

velocity :: Int -> Deci
velocity dis
    | r < 5 = fromIntegral q / 10
    | otherwise = fromIntegral (q + 1) / 10
  where
    v = (dis * 100) `div` 60
    (q, r) = divMod v 10

newtype WindPower = WindPower Int
    deriving (Eq, Ord)

instance Show WindPower where
    show (WindPower x) = show x

instance Enum WindPower where
    fromEnum (WindPower x) = x
    toEnum = WindPower

instance Bounded WindPower where
    minBound = WindPower 0
    maxBound = WindPower 12

windPower :: Int -> WindPower
windPower dis
    | v <= 0.2  = toEnum 0
    | v <= 1.5  = toEnum 1
    | v <= 3.3  = toEnum 2
    | v <= 5.4  = toEnum 3
    | v <= 7.9  = toEnum 4
    | v <= 10.7 = toEnum 5
    | v <= 13.8 = toEnum 6
    | v <= 17.1 = toEnum 7
    | v <= 20.7 = toEnum 8
    | v <= 24.4 = toEnum 9
    | v <= 28.4 = toEnum 10
    | v <= 32.6 = toEnum 11
    | otherwise = toEnum 12
  where
    v = velocity dis