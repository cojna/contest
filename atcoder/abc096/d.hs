main=readLn>>=putStrLn.unwords.(`take`[show p|p<-[11,31..],rem(2^p)p==2])
