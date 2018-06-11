import           Data.Bool
import           Data.List

main :: IO ()
main = getLine >>= putStrLn . bool "No" "Yes" . solve

solve (x:y:z:cs)
    | sort[x,y,z] == "ABC" = True
    | otherwise = solve (y:z:cs)
solve _ = False
