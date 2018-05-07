import           Data.List
main=interact$snd.maximum.map(\g->(length g,head g)).group.sort.buildDict.last.lines

buildDict :: String -> [String]
buildDict cs = go cs
  where
    go (x:y:xs) = [x, y] : go (y:xs)
    go _        = []
