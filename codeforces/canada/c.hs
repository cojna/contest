main=getLine>>=mapM_ putStrLn.solve

solve :: String -> [String]
solve cs
    | or . zipWith (==) cs $ tail cs = ["Impossible"]
    | (res0, res1)<-splitAt(div lx 2) xs
    , (res2, res3)<-splitAt(div ly 2) ys
    = [reverse res0 ++ [dup] ++ res2
      ,res1 ++ reverse res3]
  where
    [dup] = [c | c<-['A'..'Z'], sum[1|d<-cs,c==d] == 2]
    (cs0, _:cs1) = span (/=dup) cs
    (xs, _:ys) = span (/=dup) $ cs1 ++ cs0
    lx = length xs
    ly = length ys
