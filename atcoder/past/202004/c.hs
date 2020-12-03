import Control.Monad

main :: IO ()
main = do
    n <- readLn
    css <- replicateM n getLine
    putStr.unlines $ scanr1 step css

step :: String -> String -> String
step (x0:'#':x2:xs) (y0:y1:y2:ys)
    | elem 'X' [y0, y1, y2] = x0 : step ('X':x2:xs) (y1:y2:ys)
    | otherwise = x0 : step ('#':x2:xs) (y1:y2:ys)
step (x0:xs) (y0:ys) = x0 : step xs ys
step _ _ = []
