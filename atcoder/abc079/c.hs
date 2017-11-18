import           Control.Monad
import           Data.Char
main=getLine>>=putStrLn.head.f.map digitToInt
f[a,b,c,d]=do
    [(op0,s0),(op1,s1),(op2,s2)]<-mapM(const[((+),"+"),((-),"-")])[1..3]
    let res = a `op0` b `op1` c `op2` d
    guard $ res == 7
    return $ shows a s0 ++ shows b s1 ++ shows c s2 ++ shows d "=7"
