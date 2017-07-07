#this is the code for overlay youth survey onto school bounds
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

#accessing the youth survey data
#take some steps to clean up the data and name the columns
youth_results <- read_excel("data/comm_fairfax/original/2015 Supplemental Analysis by Pyramid Report__GIS.xlsx",
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

#EXAMPLE HEATMAP
#Overall heatmap for Depressive symptoms
plt <-ggplot(highSchool) +
    geom_polygon(data = highSchool.df, aes(x = long, y = lat, group = OBJECTID,
    fill = as.numeric(Depressive_Symptoms)), color = "black") +
    labs(title = "% of Students reporting Depressive Symptoms") +
    scale_fill_gradient2(low = '#19bd00', mid = '#f5f671', high = '#fd0000', midpoint = 26, labels("%")) +
suppressWarnings(print(plt))

#~~~~~~~HEATMAPS FOR 10TH and 12TH GRADES note that midpoints will be different
#10th grade depressive symptoms
plt <-ggplot(highSchool) +
    geom_polygon(data = highSchool.df_10, aes(x = long, y = lat, group = OBJECTID,
    fill = as.numeric(Depressive_Symptoms)), color = "black") +
    labs(title = "% of 10th Grade Students reporting Depressive Symptoms") +
    scale_fill_gradient2(low = '#19bd00', mid = '#f5f671', high = '#fd0000', midpoint = 24.5,
                         guide = guide_colourbar(title = "Percent"))
suppressWarnings(print(plt))
#ggsave(filename = "10th_grade_depress.png", path = "~/git/lab/comm_fairfax/data/comm_fairfax/working/Youth_Survey_Heat_Maps", device = "png")

#10th grade suicide considerations
plt <-ggplot(highSchool) +
    geom_polygon(data = highSchool.df_10, aes(x = long, y = lat, group = OBJECTID,
    fill = as.numeric(Suicide_Consider)), color = "black") +
    labs(title = "% of 10th GradeStudents considering suicide") +
    scale_fill_gradient2(low = '#19bd00', mid = '#f5f671', high = '#fd0000', midpoint = 14,
                         guide = guide_colourbar(title = "Percent"))
suppressWarnings(print(plt))
#ggsave(filename = "10th_grade_consider.png", path = "~/git/lab/comm_fairfax/data/comm_fairfax/working/Youth_Survey_Heat_Maps", device = "png")

#10th grade suicide attempts
plt <-ggplot(highSchool) +
    geom_polygon(data = highSchool.df_10, aes(x = long, y = lat, group = OBJECTID,
    fill = as.numeric(Suicide_Attempt)), color = "black") +
    labs(title = "% of 10th Grade Students attempted suicide") +
    scale_fill_gradient2(low = '#19bd00', mid = '#f5f671', high = '#fd0000', midpoint = 6,
                         guide = guide_colourbar(title = "Percent"))
suppressWarnings(print(plt))
#ggsave(filename = "10th_grade_attempt.png", path = "~/git/lab/comm_fairfax/data/comm_fairfax/working/Youth_Survey_Heat_Maps", device = "png")

#10th grade high stress
plt <-ggplot(highSchool) +
    geom_polygon(data = highSchool.df_10, aes(x = long, y = lat, group = OBJECTID,
    fill = as.numeric(Stress_High)), color = "black") +
    labs(title = "% of 10th Grade Students reporting high stress") +
    scale_fill_gradient2(low = '#19bd00', mid = '#f5f671', high = '#fd0000', midpoint = 38,
                         guide = guide_colourbar(title = "Percent"))
suppressWarnings(print(plt))
#ggsave(filename = "10th_grade_stress.png", path = "~/git/lab/comm_fairfax/data/comm_fairfax/working/Youth_Survey_Heat_Maps", device = "png")

#12th grade depressive symptoms
plt <-ggplot(highSchool) +
    geom_polygon(data = highSchool.df_12, aes(x = long, y = lat, group = OBJECTID,
    fill = as.numeric(Depressive_Symptoms)), color = "black") +
    labs(title = "% of 12th Grade Students reporting Depressive Symptoms") +
    scale_fill_gradient2(low = '#19bd00', mid = '#f5f671', high = '#fd0000', midpoint = 30,
                         guide = guide_colourbar(title = "Percent"))
suppressWarnings(print(plt))
#ggsave(filename = "12th_grade_depress.png", path = "~/git/lab/comm_fairfax/data/comm_fairfax/working/Youth_Survey_Heat_Maps", device = "png")

#12th grade suicide considerations
plt <-ggplot(highSchool) +
    geom_polygon(data = highSchool.df_12, aes(x = long, y = lat, group = OBJECTID,
    fill = as.numeric(Suicide_Consider)), color = "black") +
    labs(title = "% of 12th Grade Students considering suicide") +
    scale_fill_gradient2(low = '#19bd00', mid = '#f5f671', high = '#fd0000', midpoint = 16,
                         guide = guide_colourbar(title = "Percent"))
suppressWarnings(print(plt))
#ggsave(filename = "12th_grade_consider.png", path = "~/git/lab/comm_fairfax/data/comm_fairfax/working/Youth_Survey_Heat_Maps", device = "png")


#12th grade suicide attempts
plt <-ggplot(highSchool) +
    geom_polygon(data = highSchool.df_12, aes(x = long, y = lat, group = OBJECTID,
    fill = as.numeric(Suicide_Attempt)), color = "black") +
    labs(title = "% of 12th Grade Students attempted suicide") +
    scale_fill_gradient2(low = '#19bd00', mid = '#f5f671', high = '#fd0000', midpoint = 7,
                         guide = guide_colourbar(title = "Percent"))
suppressWarnings(print(plt))
#ggsave(filename = "12th_grade_attempt.png", path = "~/git/lab/comm_fairfax/data/comm_fairfax/working/Youth_Survey_Heat_Maps", device = "png")



#12th grade high stress
plt <-ggplot(highSchool) +
    geom_polygon(data = highSchool.df_12, aes(x = long, y = lat, group = OBJECTID,
    fill = as.numeric(Stress_High)), color = "black") +
    labs(title = "% of 12th Grade Students reporting high stress") +
    scale_fill_gradient2(low = '#19bd00', mid = '#f5f671', high = '#fd0000', midpoint = 44,
                         guide = guide_colourbar(title = "Percent"))
suppressWarnings(print(plt))
#ggsave(filename = "12th_grade_stress.png", path = "~/git/lab/comm_fairfax/data/comm_fairfax/working/Youth_Survey_Heat_Maps/", device = "png")
#~~~~~~END HEAT MAPS

#THIS IS CODE TO OVERLAY MENTAL HEALTH PROVIDER WITH OVERALL DEPRESSIVE SYMPTOM MAP
#Copy code from zarni

#THIS LINE DOESN'T WORK ALL THE TIME CHECK THE PATH
mhp_clean <- rio::import("data/comm_fairfax/original/mhp_clean.csv")
long_lat_mhp <- SpatialPoints(cbind(Long=as.numeric(mhp_clean$longitude), Lat=as.numeric(mhp_clean$latitude)))
long_lat_mhp_frame <- SpatialPointsDataFrame(cbind(lon = as.numeric(mhp_clean$longitude), lat = as.numeric(mhp_clean$latitude)),data = mhp_clean)
proj4string(long_lat_mhp_frame) <- proj4string(highSchool)
crs.geo <- CRS("+init=epsg:4326")
proj4string(long_lat_mhp) <- crs.geo
load("./data/comm_fairfax/working/latLongTherapistPlotData.RData")




nrow(highSchool@data)
#Gives Associated Polygon for each Spatial Point
sch_with_mhp <- as.data.table(over(long_lat_mhp,highSchool))
mhp_per_sch <- sch_with_mhp[,.N,OBJECTID]
mhp_per_sch <- mhp_per_sch %>% left_join(highSchool@data, by = c("OBJECTID", "OBJECTID"))

plt <-ggplot(highSchool) +
    geom_polygon(data = highSchool.df, aes(x = long, y = lat, group = OBJECTID,
                                           fill = as.numeric(Depressive_Symptoms)), color = "black") +
    labs(title = "Mental Health providers, % of Overall Students reporting Depressive Symptoms") +
    scale_fill_gradient2(low = '#19bd00', mid = '#f5f671', high = '#fd0000', midpoint = 26,
                         guide = guide_colourbar(title = "Percent")) +
    geom_point(data =latLongPlotData2, aes(x=as.numeric(longitude), y=as.numeric(latitude)),  color = "blue") +
    geom_point(data =mhp_clean, aes(x=as.numeric(longitude), y=as.numeric(latitude)),  color = "deepskyblue")

suppressWarnings(print(plt))
#ggsave(filename = "mh_depress_overlay.png", path = "~/git/lab/comm_fairfax/data/comm_fairfax/working/Youth_Survey_Heat_Maps/overlays", device = "png")

