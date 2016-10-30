import           Data.List
main=getContents>>=print.f.tail.lines
f[s,t]=minimum[length s + length t - length x | x<-inits t, x `isSuffixOf` s]
