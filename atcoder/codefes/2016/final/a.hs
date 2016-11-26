main=getContents>>=putStrLn
    .fst
    .head
    .filter((=="snuke").snd)
    .concat
    .zipWith(\i l->[(c:show i,s)|(c,s)<-l])[1..]
    .map(zip['A'..].words)
    .tail
    .lines
