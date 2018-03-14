main :: IO ()
main = getLine >>= print . length . filter (=='1')
