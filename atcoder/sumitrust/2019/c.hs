import Data.Bits
import Data.List

main :: IO ()
main = do
    x <- readLn
    let table = buildTable 1000
    if testBit table x
    then print 1
    else print 0


buildTable :: Int -> Integer
buildTable n = foldl' step 1 [1..n]
  where
    step x _
        = foldl' (.|.) x
        $ map (shiftL x) [100..105]

