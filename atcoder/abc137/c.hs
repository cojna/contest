import Data.List;main=interact$show.f.lines;f l=sum$do g<-group.sort$sort<$>l;[1..length g-1]