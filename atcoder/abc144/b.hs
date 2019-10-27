main=do
  x<-readLn
  if elem x l
  then putStrLn"Yes"
  else putStrLn"No"
l=[x*y|x<-[1..9],y<-[1..9]]