main=interact$show.f.(!!1).lines
f cs = g cs + h cs
g cs = length $ takeWhile (=='<') cs
h cs = length . takeWhile (=='>') $ reverse cs
