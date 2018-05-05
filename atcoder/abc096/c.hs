import           Data.Array.Unboxed
import           Data.Bool

main :: IO ()
main = do
    [h, w] <- map read.words <$> getLine
    css <- lines <$> getContents
    putStrLn . bool "No" "Yes" $ solve h w css

solve :: Int -> Int -> [String] -> Bool
solve h w css = and[isValid (i, j)|i<-[0..h-1],j<-[0..w-1]]
  where
    m :: UArray (Int, Int) Char
    m = listArray ((0, 0), (h - 1, w - 1)) $ concat css
    next (x, y) = [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]
    inGrid (x,y) = 0 <= x && x < h && 0 <= y && y < w
    neighbors = filter inGrid . next
    isValid (x, y) = m ! (x, y) == '.' || any (\(nx, ny)-> m ! (nx, ny) == '#') (neighbors (x, y))
