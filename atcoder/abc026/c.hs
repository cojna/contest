main=getContents>>=print.(#1).tail.map read.words;l#v|notElem v l=1|c<-[l#i|(i,p)<-zip[2..]l,p==v]=maximum c+minimum c+1