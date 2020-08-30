import Control.Monad
import Data.List

main :: IO ()
main = do
    [n,a,b] <- map read.words <$> getLine
    sds <- replicateM n $ do
        [s, d] <- words <$> getLine
        return (read s, read d)
    let pos = foldl'
            (\acc (s, d) -> case s of
                East -> acc + d `max` a `min` b
                West -> acc - d `max` a `min` b
            ) 0 sds
    if pos < 0
    then putStrLn $ unwords [show West, show (-pos)]
    else if pos == 0
    then print 0
    else putStrLn $ unwords [show East, show pos]

data Dir = East | West deriving (Show, Read)
