import Data.List
main=interact$(>>=snd).sort.p 1.tail.words
p i(x:y:l)=((x,-read y),shows i"\n"):p(i+1)l
p _[]=[]