library(ggmap)
library(stringr)

therapistAddress <- as.data.frame(read.csv("~/git/comm_fairfax/data/comm_fairfax/original/masterAddress.txt"))
LatLong<-vector()

for(i in 1:1080)
{
  LatLong[i] <- paste(rev(as.character(geocode(as.character(therapistAddress[i,1])))), collapse = ' ')
  print(i)
}


## @Eirik - should be able to put these snippets inside a for loop (i in 1:1080) with LatLong loaded in the environment

strsplit(LatLong[i], " ")[[1]][1] #returns the latitude of ith entry

strsplit(LatLong[i], " ")[[1]][2] #returns the longitude of ith entry
