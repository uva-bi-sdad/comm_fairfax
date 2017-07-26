#this is the code for overlaying mental health providers by client focus onto the school pyramid boundary map filled by
#percentage of students reporting depressive symptoms.
library(rgdal)
library(maptools)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(reshape2)
library(scales)
#library(OpenStreetMap)
library(osmar)
library(ggmap)
library(raster)
#library(rasterVis)
library(data.table)
library(RColorBrewer)
library(sp)
library(readxl)
library(rio)

#~~~~~~~~~~~start copying the geographic code from zarni for GIS
mgmap <- get_map(location=c(-77.7173, 38.5976, -76.8686, 39.0682), source = "google", color = "bw")
vgmap <- as.vector(mgmap)
vgmaprgb <- col2rgb(vgmap)
gmapr <- matrix(vgmaprgb[1, ], ncol = ncol(mgmap), nrow = nrow(mgmap))
gmapg <- matrix(vgmaprgb[2, ], ncol = ncol(mgmap), nrow = nrow(mgmap))
gmapb <- matrix(vgmaprgb[3, ], ncol = ncol(mgmap), nrow = nrow(mgmap))
rgmaprgb <- brick(raster(gmapr), raster(gmapg), raster(gmapb))
rm(gmapr, gmapg, gmapb)
projection(rgmaprgb) <- CRS("+init=epsg:4326")
proj4string(rgmaprgb) #Checking the Projection String
extent(rgmaprgb) <- unlist(attr(mgmap, which = "bb"))[c(2, 4, 1, 3)]
rgmaprgb

#still copying code from zarni for GIS

setwd("~/sdal/projects/limbo/fairfax_alerts/")
#County Shape File
county <- readShapePoly("GISData/Fairfax_County_Border/Fairfax_County_Border.shp",
                        proj4string=CRS('+proj=longlat +ellps=WGS84'))
#Zip Shape File
zip <- readShapePoly("GISData/ZIP_Codes/ZIP_Codes.shp",
                     proj4string=CRS('+proj=longlat +ellps=WGS84'))

#High School Shape File
highSchool <- readShapePoly("GISData/High_School_Pyramids/High_School_Attendance_Areas.shp",
                            proj4string=CRS('+proj=longlat +ellps=WGS84'))

#Convert Everything to the coordinate system for the raster map
county <- spTransform(county,rgmaprgb@crs)
zip <- spTransform(zip,rgmaprgb@crs)
highSchool <- spTransform(highSchool, rgmaprgb@crs)
par(mfrow=c(1,1))
plot(highSchool, main = "Fairfax High School Boundary")
#~~~~~~~~~~ end of copying zarni's code for GIS stuff

#accessing the youth survey data
#take some steps to clean up the data and name the columns
youth_results <- read_excel("~/git/comm_fairfax/data/comm_fairfax/original/2015 Supplemental Analysis by Pyramid Report__GIS.xlsx",
                            sheet = "8-10-12 Results by Pyramid")
youth_results <- youth_results[-1,]
colnames(youth_results) <- youth_results[1,]
youth_results <- youth_results[-1,]

#we only select the columns most related to mental health issues
youth_results_mh <- youth_results[c('Pyramid_Number', 'Pyramid', 'Demographic', 'Depressive_Symptoms',
                                    'Suicide_Consider','Suicide_Attempt','Stress_Low','Stress_Medium','Stress_High')]

#make different data sets depending on grade level (chanida will do 8)
youth_results_mh_overall <- subset(youth_results_mh, Demographic == "Overall")
youth_results_mh_10 <- subset(youth_results_mh, Demographic == "10th Grade")
youth_results_mh_12 <- subset(youth_results_mh, Demographic == "12th Grade")


#joining the survey results to the highSchool map data for each grade level
highSchool_percent_count <- youth_results_mh_overall %>% left_join(highSchool@data, by = c("Pyramid" = "SCHOOL_NAM"))
highSchool_percent_count_10 <- youth_results_mh_10 %>% left_join(highSchool@data, by = c("Pyramid" = "SCHOOL_NAM"))
highSchool_percent_count_12 <- youth_results_mh_12 %>% left_join(highSchool@data, by = c("Pyramid" = "SCHOOL_NAM"))

#~~~~~~~~~Based on Bianica's Code
names(highSchool@data)
highSchool@data$id <- rownames(highSchool@data)
highSchool@data$id

#Convert Polygon information into data frame is what fortify does
highSchool.points <- fortify(highSchool)
names(highSchool.points)

#we are joining the Lat Long data with our shape data that we joined in line 71
#do this three times: once for each grade level
highSchool.df <- left_join(highSchool.points, highSchool@data, by = "id")
highSchool.df <- left_join(highSchool.df, highSchool_percent_count, by = "OBJECTID")

highSchool.df_10 <- left_join(highSchool.points, highSchool@data, by = "id")
highSchool.df_10 <- left_join(highSchool.df_10, highSchool_percent_count_10, by = "OBJECTID")

highSchool.df_12 <- left_join(highSchool.points, highSchool@data, by = "id")
highSchool.df_12 <- left_join(highSchool.df_12, highSchool_percent_count_12, by = "OBJECTID")



#THIS IS CODE TO OVERLAY MENTAL HEALTH PROVIDER WITH OVERALL DEPRESSIVE SYMPTOM MAP
#Copy code from zarni

#THIS LINE DOESN'T WORK ALL THE TIME CHECK THE WORKING DIRECTORY
mhp_clean <- rio::import("~/git/comm_fairfax/data/comm_fairfax/original/mhp_clean.csv")
privateTeen <- public_private_MHP[grep("Teenager", public_private_MHP$clientGroup),]


long_lat_privateTeen <- SpatialPoints(cbind(Long=as.numeric(privateTeen$longitude), Lat=as.numeric(privateTeen$latitude)))
long_lat_public <- SpatialPoints(cbind(Long=as.numeric(mhp_clean$longitude), Lat=as.numeric(mhp_clean$latitude)))
proj4string(long_lat_privateTeen) <- proj4string(highSchool)
proj4string(long_lat_public) <- proj4string(highSchool)


long_lat_mhp_frame <- SpatialPointsDataFrame(cbind(lon = as.numeric(mhp_clean$longitude), lat = as.numeric(mhp_clean$latitude)),data = mhp_clean)
proj4string(long_lat_privateTeen) <- proj4string(highSchool)

crs.geo <- CRS("+init=epsg:4326")
proj4string(long_lat_mhp) <- crs.geo

sch_with_private <- as.data.table(over(long_lat_privateTeen,highSchool))
sch_with_public <-  as.data.table(over(long_lat_public,highSchool))

View(sch_with_private)
View(sch_with_public)

sch_with_public$type <- "Public"
sch_with_private$type <- "Private - Teen Focus"

mhpCount <- rbind(sch_with_private, sch_with_public)

mhpCount1 <- mhpCount[complete.cases(mhpCount$SCHOOL_NAM),]




print(mhpCountHist)

masterTherapistDF3 <- data.frame()
DF4 <- data.frame()
load("~/git/comm_fairfax/data/comm_fairfax/working/fairfaxTherapistMaster.RData")
sub <- apply(masterTherapistDF2,1,function(x){any( is.na(x))})
DF4 <- masterTherapistDF2[!sub,]
sub1 <- apply(DF4[,4:5],1,function(x){any(((x >= 39.0682) | ((x <= 38.5976) & (x >= 0))) | ((x[2]<= -77.55) | ((x[2] >= -77.05) & (x[2] <= 25))))})
masterTherapistDF3 <- DF4[!sub1,]
masterTherapistDF3$clientGroup <- NA
mhp_clean <- rio::import("~/git/comm_fairfax/data/comm_fairfax/original/mhp_clean.csv")

for(i in 1:length(masterTherapistDF3$clientGroup))
{
  if   (length(grep("Teenagers", masterTherapistDF3[i,3]))==1)
  {
    masterTherapistDF3$clientGroup[i] <- "Teenager"
  }
  else if(length(grep("Tween",masterTherapistDF3[i,3]))==1)
  {
    masterTherapistDF3$clientGroup[i] <- "Pre-teen"
  }
  else if(length(grep("Children",masterTherapistDF3[i,3])) ==1)
  {
    masterTherapistDF3$clientGroup[i] <- "Children"
  }

  else
  {masterTherapistDF3$clientGroup[i] <- "Adults"

  }
}
save(masterTherapistDF3, file= "~/git/comm_fairfax/data/comm_fairfax/working/fairfaxTherapistMaster.RData")

nrow(highSchool@data)
#Gives Associated Polygon for each Spatial Point
sch_with_mhp <- as.data.table(over(long_lat_mhp,highSchool))
mhp_per_sch <- sch_with_mhp[,.N,OBJECTID]
mhp_per_sch <- mhp_per_sch %>% left_join(highSchool@data, by = c("OBJECTID", "OBJECTID"))

publicMHP<-as.data.frame(mhp_clean[,8:9])
publicMHP$clientGroup <- "Public"
publicMHP$Address <- NA
publicMHP$Type <- NA
publicMHP$`Client Focus` <- NA
public_private_MHP <- rbind(publicMHP, masterTherapistDF3)

public_private_MHP<-public_private_MHP[- grep("Adults", public_private_MHP$clientGroup),]
View(public_private_MHP)

png("~/git/comm_fairfax/data/comm_fairfax/working/Youth_Survey_Heat_Maps/serviceProviders.png", height = 2000, width = 2000)
plt <- ggplot(highSchool) +
  geom_polygon(data = highSchool.df, aes(x = long, y = lat, group = OBJECTID,
                                         fill = as.numeric(Depressive_Symptoms)), color = "black") +
  scale_fill_gradient2(low = '#19bd00', mid = '#f5f671', high = '#fd0000', midpoint = 26,
                       guide = guide_colourbar(title = "Percent")) +
  theme(axis.title=element_text(size=42),axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())+

  #geom_point(data =latLongPlotData2, aes(x=as.numeric(longitude), y=as.numeric(latitude)),  color = "blue") +
  geom_point(data =public_private_MHP, aes(x=as.numeric(longitude), y=as.numeric(latitude), color=factor(clientGroup)), size = 3) +

  scale_colour_manual(values=c("cyan3", "magenta", "gold", "black"),
                    name="Client Age Focus (private) / Public",
                    limits=c("Teenager", "Pre-Teen", "Children", "Public"),
                    labels=c("Teenagers", "Pre-teens", "Children", "Public"))

  print(plt)
dev.off()
ggsave(plt, filename = "heatMapTherapist.png",path = "~/git/comm_fairfax/output/",device = "png", width=20,height=11.25,scale=1)

mhpCountHist <- ggplot(data = mhpCount1, aes(mhpCount1$SCHOOL_NAM, fill = mhpCount1$type)) + geom_histogram(stat = "count") +
  labs(x="School Name", y="Count") +
  theme_minimal() + scale_fill_discrete(name = "Type") +
  theme(axis.title= element_text(size = 24), axis.text.x=element_text(angle=51,hjust=1, size=20),
        axis.text.y=element_text(angle=0,hjust=1, size=20)) +
  scale_x_discrete(limits= c("ANNANDALE", "FALLS CHURCH", "MCLEAN","MARSHALL", "STUART","THOMAS JEFFERSON", "EDISON","HAYFIELD", "LEE", "MOUNT VERNON", "WEST POTOMAC", "CHANTILLY", "FAIRFAX","WESTFIELD","WOODSON","CENTREVILLE","LAKE BRADDOCK","ROBINSON","SOUTH COUNTY", "WEST SPRINGFIELD", "HERNDON", "LANGLEY","MADISON","OAKTON","SOUTH LAKES"))

suppressWarnings(print(plt))
suppressWarnings(print(mhpCountHist))


ggsave(mhpCountHist, filename = "MHPbyschool_count.png",
  path = "~/git/comm_fairfax/output/",
device = "png", width=20,height=11.25,scale=1)
ggsave(plt, filename = "serviceProviders.png",
       path = "~/git/comm_fairfax/output/",
       device = "png", width=20,height=11.25,scale=1)

ggsave(filename = "mh_depress_overlay.png", path = "~/git/comm_fairfax/data/comm_fairfax/working/Youth_Survey_Heat_Maps/overlays", device = "png")

