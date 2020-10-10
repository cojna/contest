import Control.Monad
import Data.List

main :: IO ()
main = do
    _ <- getLine
    cs <- getLine
    print . length $ do
        x <- ['0'..'9']
        y <- ['0'..'9']
        z <- ['0'..'9']
        guard $ [x,y,z] `isSubsequenceOf` cs
        return ()
