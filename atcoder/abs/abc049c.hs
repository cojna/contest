import           Data.Bool
import           Data.List

main :: IO ()
main = do
    cs <- getLine
    putStrLn . bool "NO" "YES" $ solve cs

solve :: String -> Bool
solve cs = go $ reverse cs
  where
    go "" = True
    go cs = case [d | d<-dict, d `isPrefixOf` cs] of
        (d:_) -> go $ drop (length d) cs
        []    -> False

dict :: [String]
dict = map reverse ["dream", "dreamer", "erase", "eraser"]
