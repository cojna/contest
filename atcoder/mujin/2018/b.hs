import           Data.Bool

main :: IO ()
main = do
  a <- readLn
  cs <- getLine
  putStrLn.bool"No""Yes"$solve a cs

solve :: Int -> String -> Bool
solve a cs = elem 0 $ scanl step a cs
  where
     step acc '+' = acc + 1
     step acc '-' = acc - 1
