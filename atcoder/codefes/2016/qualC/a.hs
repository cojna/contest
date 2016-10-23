import           Data.Bool
main=getLine>>=putStrLn.bool"No""Yes".f.dropWhile(=='F').filter(`elem`"CF")
f('C':cs)=elem 'F' cs
f _ = False
