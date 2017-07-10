# for a given zip code, draw a lat+long uniformly from the region

library(maptools)
library(dplyr)
library(tigris)
library(sp)

ffx_person <- read.csv("~/mounts/lightfoot/sdal/projects/comm_fairfax/working/pums_all_fairfax.csv")[,-1]

# read in high school boundaries, zip code boundaries
#zip <- readShapePoly("~/mounts/lightfoot/sdal/projects/limbo/fairfax_alerts/GISData/ZIP_Codes/ZIP_Codes.shp",
#                     proj4string=CRS('+proj=longlat +ellps=WGS84'))

zip <- zctas(state="Virginia")
highSchool <- readShapePoly("~/mounts/lightfoot/sdal/projects/limbo/fairfax_alerts/GISData/High_School_Pyramids/High_School_Attendance_Areas.shp",
                            proj4string=CRS('+proj=longlat +ellps=WGS84'))

# read in ZCTA boundaries for Fairfax
# should be 47 w/ population (2 without)

plot(zip2)
plot(highSchool,add=TRUE,lty=2,col="red")


zipdat <- as.data.frame(table(ffx_person$ZCTAS))
names(zipdat) <- c("zip","n")

# zipdat$zip %in% zip@data$ZCTA5CE10
save.image("add_latlongs_highschool.RData")

ids <- which( zip@data$ZCTA5CE10 %in% zipdat$zip )
zip2 <- zip[ids,]

plot(highSchool,lty=4)
plot(zip2,add=TRUE,lty=1)
# looks good
rm(zip)

# --------------------------------------------------

#setwd("~/Desktop/Synthetic Pop/Application Change of Support/")

load("add_latlongs_highschool.RData")
library(maptools)
library(dplyr)
library(tigris)
library(sp)


# draw lat, long from within each zip code boundary
# create data frame containing zip names, numbers

# loop over data frame; draw n samples from each zip
# supset 'zip' poly to a single zip code, and draw from it
latlong <- data.frame(latitude=NA, longitude=NA)[-1,]
for(i in 1:nrow(zipdat)){
  ids <- which( zip2@data$ZCTA5CE10 == zipdat$zip[i] )
  zip_sub <- zip2[ids,]
  #plot(zip_sub)
  samp <- spsample(x=zip_sub,n=zipdat$n[i],type="random")
  # extract (lat, long) from samp and cbind it
  latlong <- rbind(latlong, samp@coords)
}

ffx_person$long <- latlong$x
ffx_person$lat <- latlong$y

# attach high school areas for each lat, long
pts <- SpatialPoints(latlong, proj4string=CRS('+proj=longlat +ellps=WGS84'))
regions <- over(pts,highSchool)
table(regions$SCHOOL_NAM,useNA = "always")
sum(is.na(regions$SCHOOL_NAM))/nrow(regions)*100
# 24,000 (2%) of people in these zip codes fall outside of a high school region; keep them as NA

ffx_person$HighSchool <- regions$SCHOOL_NAM

# save image w/ lat, long, high school region
write.csv(ffx_person,"~/mounts/lightfoot/sdal/projects/comm_fairfax/working/pums_all_fairfax_latlong.csv")
