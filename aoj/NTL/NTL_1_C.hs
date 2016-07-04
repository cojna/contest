-- main=interact$(`shows`"\n").foldr(lcm.read)1.tail.words
main=interact$(++"\n").show.foldr(lcm.read)1.tail.words
