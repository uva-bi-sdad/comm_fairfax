#This is a continuation of youth survey mapping. mostly overlay maps here
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

#read the county parks shape file
parks <- readShapePoly("~/git/lab/comm_fairfax/data/comm_fairfax/original/ffx_parks/County_Parks.shp",
                              proj4string=CRS('+proj=longlat +ellps=WGS84'))
parks <- spTransform(parks, rgmaprgb@crs)
plot(parks)

#read the non county parks shape files
nonparks <- readShapePoly("~/git/lab/comm_fairfax/data/comm_fairfax/original/ffx_parks/NonCounty_Parks.shp",
                       proj4string=CRS('+proj=longlat +ellps=WGS84'))
nonparks <- spTransform(nonparks, rgmaprgb@crs)
plot(nonparks)
#~~~~~~~~~~~~~start import old code for OVERALL youth survey to overlay
#accessing the youth survey data
#take some steps to clean up the data and name the columns
youth_results <- read_excel("~/git/lab/comm_fairfax/data/comm_fairfax/original/2015 Supplemental Analysis by Pyramid Report__GIS.xlsx",
                            sheet = "8-10-12 Results by Pyramid")
youth_results <- youth_results[-1,]
colnames(youth_results) <- youth_results[1,]
youth_results <- youth_results[-1,]

#we only select the columns most related to mental health issues
youth_results_mh <- youth_results[c('Pyramid_Number', 'Pyramid', 'Demographic', 'Depressive_Symptoms',
                                    'Suicide_Consider','Suicide_Attempt','Stress_Low','Stress_Medium','Stress_High')]

#make different data sets depending on grade level (chanida will do 8)
youth_results_mh_overall <- subset(youth_results_mh, Demographic == "Overall")

#joining the survey results to the highSchool map data for each grade level
highSchool_percent_count <- youth_results_mh_overall %>% left_join(highSchool@data, by = c("Pyramid" = "SCHOOL_NAM"))

#Based on Bianica's Code
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
#~~~~~~~~~end of import old code for OVERALL high school data

#################
################# START OF NEW OVERLAYS ON TOP OF YOUTH SURVEY HEATMAPS
#start with overlaying COUNTY PARKS on OVERALL DEPRESSIVE SYMPTOMS
#add county parks
parks@data$id <- rownames(parks@data)
parks.points <- fortify(parks)
names(parks.points)

parks.df <- left_join(parks.points, parks@data, by = "id")

#add non county parks
nonparks@data$id <- rownames(nonparks@data)
nonparks.points <- fortify(nonparks)
names(nonparks.points)

nonparks.df <- left_join(nonparks.points, nonparks@data, by = "id")

#do a ggplot on top of youth survey data
plt <-ggplot(highSchool) +
    geom_polygon(data = highSchool.df, aes(x = long, y = lat, group = OBJECTID,
                                           fill = as.numeric(Depressive_Symptoms)), color = "black") +
    labs(title = "Parks in Fairfax, % of Overall Students reporting Depressive Symptoms") +
    scale_fill_gradient2(low = '#19bd00', mid = '#f5f671', high = '#fd0000', midpoint = 26,
                         guide = guide_colourbar(title = "Percent")) +
    geom_polygon(data = parks.df, aes(x=long, y=lat, group = group)) +
    geom_polygon(data = nonparks.df, aes(x=long, y=lat, group = group, color = 'red'))
suppressWarnings(print(plt))
#use this to save plot
#ggsave(filename = "parks_depress_overlay.png", path = "data/comm_fairfax/working/Youth_Survey_Heat_Maps/overlays")
