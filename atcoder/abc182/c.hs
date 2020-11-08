import Data.Char
import Data.List

main :: IO ()
main = do
    cs <- getLine
    let d = length cs
    let ds = map ((`mod`3).digitToInt) cs
    let cnt0 = sum[1 | 0<-ds]
        cnt1 = sum[1 | 1<-ds]
        cnt2 = sum[1 | 2<-ds]
    case mod (sum ds) 3 of
        0 -> print 0
        1 | cnt1 > 0, d > 1 -> print 1
          | cnt2 >= 2, d > 2 -> print 2
        2 | cnt2 > 0, d > 1 -> print 1
          | cnt1 >= 2, d > 2 -> print 2
        _ -> print (-1)
