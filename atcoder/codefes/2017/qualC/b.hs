main=getContents>>=print.f.map read.words
f(n:xs) = ((3^n) - ).product $ 2 <$ filter even xs

