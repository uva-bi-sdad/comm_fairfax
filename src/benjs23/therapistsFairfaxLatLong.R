library(ggmap)
library(stringr)

therapistAddress <- as.data.frame(read.csv("~/git/comm_fairfax/data/comm_fairfax/original/masterAddress.txt"))
LatLong<-vector()

for(i in 1:1080)
{
  LatLong[i] <- paste(rev(as.character(geocode(as.character(therapistAddress[i,1])))), collapse = ' ')
  print(i)
}

latLongPlotData <- data.frame(matrix(ncol=2,nrow=2))
colnames(latLongPlotData) = c("latitude", "longitude")
## @Eirik - should be able to put these snippets inside a for loop (i in 1:1080) with LatLong loaded in the environment
for(j in 1:1080)
{
latLongPlotData[j, 1] <- as.numeric(strsplit(LatLong[j], " ")[[1]][1]) #returns the latitude of ith entry

latLongPlotData[j,2] <- as.numeric(strsplit(LatLong[j], " ")[[1]][2]) #returns the longitude of ith entry
}

latLongPlotData$latitude <- as.numeric(latLongPlotData$latitude)
latLongPlotData$longitude <- as.numeric(latLongPlotData$longitude)




sub <- apply(latLongPlotData,1,function(x){any( is.na(x))})
latLongPlotData1 <- latLongPlotData[!sub,]

sub1 <- apply(latLongPlotData1,1,function(x){any(((x >= 39.0682) | ((x <= 38.5976) & (x >= 0))) | ((x[2]<= -77.55) | ((x[2] >= -77.05) & (x[2] <= 25))))})
latLongPlotData2 <- latLongPlotData1[!sub1,]


save(latLongPlotData2, file ="~/git/comm_fairfax/data/comm_fairfax/working/latLongTherapistPlotData.RData")
