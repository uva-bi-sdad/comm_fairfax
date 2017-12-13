##### more overlays
# putting the overall plots together
library(dplyr)
library(rio)
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
library(gridExtra)



#Accessing the survey table
HS_Pyramid_Report <- read_excel("~/git/comm_fairfax/data/comm_fairfax/working/2015 Supplemental Analysis by Pyramid Report__GIS.xlsx",
                                sheet = "8-10-12 Results by Pyramid")

#Making the new data table for the specific catergories
HS_Pyramid_Report1 <- HS_Pyramid_Report[,c(1,2,3,
                                           90,91,92,
                                           93,94,95,
                                           105,72,73,101,106,134,135,136,
                                           20,21,126,
                                           88,89,138,140,67,48,52,61)]

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
HS_Pyramid_Report1 <- HS_Pyramid_Report1[-1,]
colnames(HS_Pyramid_Report1) <- HS_Pyramid_Report1[1,]
HS_Pyramid_Report1 <- HS_Pyramid_Report1[-1,]

#we only select the columns most related to mental health issues
HS_Pyramid_Report1_mh <- HS_Pyramid_Report1[c('Pyramid_Number', 'Pyramid', 'Demographic', 'Depressive_Symptoms',
                                              'Suicide_Consider','Suicide_Attempt','Stress_Low','Stress_Medium','Stress_High',
                                              'Binge_Drinking','BulliedVic_School','BulliedVic_Not_School','Cigarette_30',
                                              'Marijuana_30','Physical_Activity_None','Physical_Activity_Daily',
                                              'Extracurricular_Available','Extracurricular_Regularly','Fruit_Veg_5','Cyberbullying_SchoolVictim',
                                              'Cyberbullying_SchoolAggressor','Sleep_4or less', 'Sleep_6','Parent_Help_Available','Adults_Talk','Gratitude','Food_Insecurity')]
#we only select the rows where they give us the overall score of the pyramid
HS_Pyramid_Report1_mh_overall <- subset(HS_Pyramid_Report1_mh, Demographic == "Overall")


#joining the survey results to the highSchool map data
highSchool_percent_count <- HS_Pyramid_Report1_mh_overall %>% left_join(highSchool@data, by = c("Pyramid" = "SCHOOL_NAM"))

#~~~~~~~~~Based on Bianica's Code
#Based on Bianica's Code
names(highSchool@data)
highSchool@data$id <- rownames(highSchool@data)
highSchool@data$id

#Convert Polygon information into data frame is what fortify does
highSchool.points <- fortify(highSchool)
names(highSchool.points)

highSchool.df <- left_join(highSchool.points, highSchool@data, by = "id")
highSchool.df <- left_join(highSchool.df, highSchool_percent_count, by = "OBJECTID")

#CAN WE CONFIRM THE NUMBERS BY LOOKING AT THE MAP (FROM BEN: ONLINE), the table we built MHP table above and the DENSITY VOLUMES HERE?

#THIS IS CODE TO OVERLAY MENTAL HEALTH PROVIDER WITH OVERALL DEPRESSIVE SYMPTOM MAP
#Copy code from zarni
mhp_clean <- rio::import("~/git/lab/comm_fairfax/data/comm_fairfax/original/mhp_clean.csv")
long_lat_mhp <- SpatialPoints(cbind(Long=as.numeric(mhp_clean$longitude), Lat=as.numeric(mhp_clean$latitude)))
long_lat_mhp_frame <- SpatialPointsDataFrame(cbind(lon = as.numeric(mhp_clean$longitude), lat = as.numeric(mhp_clean$latitude)),data = mhp_clean)
proj4string(long_lat_mhp_frame) <- proj4string(highSchool)
crs.geo <- CRS("+init=epsg:4326")
proj4string(long_lat_mhp) <- crs.geo

nrow(highSchool@data)

##################START
#Overall heatmap for Depressive symptoms
plt <-ggplot(highSchool) +
    geom_polygon(data = highSchool.df, aes(x = long, y = lat, group = OBJECTID,
                                           fill = as.numeric(Depressive_Symptoms)), color = "black") +
    labs(title = "% of Students reporting Depressive Symptoms") +
    scale_fill_gradient2(low = '#19bd00', mid = '#f5f671', high = '#fd0000', midpoint = 26, labels("%"))
suppressWarnings(print(plt))

#overall suicide considerations
plt2 <-ggplot(highSchool) +
    geom_polygon(data = highSchool.df, aes(x = long, y = lat, group = OBJECTID,
                                              fill = as.numeric(Suicide_Consider)), color = "black") +
    labs(title = "% of Overall Students considering suicide") +
    scale_fill_gradient2(low = '#19bd00', mid = '#f5f671', high = '#fd0000', midpoint = 14,
                         guide = guide_colourbar(title = "Percent"))
suppressWarnings(print(plt2))

#Overall suicide attempts
plt3 <-ggplot(highSchool) +
    geom_polygon(data = highSchool.df, aes(x = long, y = lat, group = OBJECTID,
                                              fill = as.numeric(Suicide_Attempt)), color = "black") +
    labs(title = "% of Overall Grade Students attempted suicide") +
    scale_fill_gradient2(low = '#19bd00', mid = '#f5f671', high = '#fd0000', midpoint = 6,
                         guide = guide_colourbar(title = "Percent"))
suppressWarnings(print(plt3))

#Overall high stress
plt4 <-ggplot(highSchool) +
    geom_polygon(data = highSchool.df, aes(x = long, y = lat, group = OBJECTID,
                                              fill = as.numeric(Stress_High)), color = "black") +
    labs(title = "% of Overall Grade Students reporting high stress") +
    scale_fill_gradient2(low = '#19bd00', mid = '#f5f671', high = '#fd0000', midpoint = 38,
                         guide = guide_colourbar(title = "Percent"))
suppressWarnings(print(plt4))

#putting the four plots together
par(mfrow=c(2,2))
grid.arrange(plt,plt2,plt3,plt4)
