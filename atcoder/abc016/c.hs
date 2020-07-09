main=interact$unlines.map show.f.map(map read.words).lines
f([n,m]:l)|e<-l++map reverse l=[sum[1|j<-[1..n],i/=j,notElem[i,j]e,or[elem[i,k]e&&elem[j,k]e|k<-[1..n]]]|i<-[1..n]]