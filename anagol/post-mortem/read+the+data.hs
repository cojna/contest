r=readLn
e n=[1..n]
main=r>>=mapM(\_->r>>=mapM(\_->r).e).e>>=putStr.(>>=max", ".pure).show