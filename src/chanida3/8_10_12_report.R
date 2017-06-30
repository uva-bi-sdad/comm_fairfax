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



#Accessing the survey table
HS_Pyramid_Report <- read_excel("~/git/comm_fairfax/data/comm_fairfax/original/2015 Supplemental Analysis by Pyramid Report__GIS.xlsx",
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
HS_Pyramid_Report1_mh_8th_grade <- subset(HS_Pyramid_Report1_mh, Demographic == "8th Grade")


#joining the survey results to the highSchool map data
highSchool_percent_count <- HS_Pyramid_Report1_mh_overall %>% left_join(highSchool@data, by = c("Pyramid" = "SCHOOL_NAM"))
highSchool_percent_count8 <- HS_Pyramid_Report1_mh_8th_grade %>% left_join(highSchool@data, by = c("Pyramid" = "SCHOOL_NAM"))

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

highSchool.df8 <- left_join(highSchool.points, highSchool@data, by = "id")
highSchool.df8 <- left_join(highSchool.df8, highSchool_percent_count8, by = "OBJECTID")

#CAN WE CONFIRM THE NUMBERS BY LOOKING AT THE MAP (FROM BEN: ONLINE), the table we built MHP table above and the DENSITY VOLUMES HERE?

#THIS IS CODE TO OVERLAY MENTAL HEALTH PROVIDER WITH OVERALL DEPRESSIVE SYMPTOM MAP
#Copy code from zarni
mhp_clean <- rio::import("~/git/comm_fairfax/data/comm_fairfax/original/mhp_clean.csv")
long_lat_mhp <- SpatialPoints(cbind(Long=as.numeric(mhp_clean$longitude), Lat=as.numeric(mhp_clean$latitude)))
long_lat_mhp_frame <- SpatialPointsDataFrame(cbind(lon = as.numeric(mhp_clean$longitude), lat = as.numeric(mhp_clean$latitude)),data = mhp_clean)
proj4string(long_lat_mhp_frame) <- proj4string(highSchool)
crs.geo <- CRS("+init=epsg:4326")
proj4string(long_lat_mhp) <- crs.geo

nrow(highSchool@data)
#Gives Associated Polygon for each Spatial Point
sch_with_mhp <- as.data.table(over(long_lat_mhp,highSchool))
mhp_per_sch <- sch_with_mhp[,.N,OBJECTID]
mhp_per_sch <- mhp_per_sch %>% left_join(highSchool@data, by = c("OBJECTID", "OBJECTID"))

#JEFFERSON HIGH SCHOOL IS IN THE LOW TRIANGLE
##################FOR THE OVERALL ROW
#HEAT MAP for % of Students reporting Depressive Symptoms
plt <-ggplot(highSchool) +
    geom_polygon(data = highSchool.df, aes(x = long, y = lat, group = OBJECTID,
    fill = as.numeric(Depressive_Symptoms)), color = "black") +
    labs(title = "% of Students reporting Depressive Symptoms") +
    scale_fill_gradient2(low = '#19bd00', mid = '#f5f671', high = '#fd0000', midpoint =25.93,
                         guide=guide_colourbar(title = "Percent")) +
    geom_point(data = mhp_clean, aes(x=as.numeric(longitude), y=as.numeric(latitude)),  color = "blue")
suppressWarnings(print(plt))
ggsave(filename = "Overall_Depressive_Symptoms.png", path = "~/git/comm_fairfax/data/comm_fairfax/working/Youth_Survey_Heat_Maps/", device = "png")


#HEAT MAP for % of Students reporting Considering Suicide
plt <-ggplot(highSchool) +
    geom_polygon(data = highSchool.df, aes(x = long, y = lat, group = OBJECTID,
    fill = as.numeric(Suicide_Consider)), color = "black") +
    labs(title = "% of Students reporting Considering Suicide") +
    scale_fill_gradient2(low = '#19bd00', mid = '#f5f671', high = '#fd0000', midpoint =15.17,
                         guide=guide_colourbar(title = "Percent"))
suppressWarnings(print(plt))
ggsave(filename = "Overall_Considering_Suicide.png", path = "~/git/comm_fairfax/data/comm_fairfax/working/Youth_Survey_Heat_Maps/", device = "png")

#HEAT MAP for Suicide Attempt
plt <-ggplot(highSchool) +
    geom_polygon(data = highSchool.df, aes(x = long, y = lat, group = OBJECTID,
    fill = as.numeric(Suicide_Attempt)), color = "black") +
    labs(title = "% of Students reporting Suicide Attempt") +
    scale_fill_gradient2(low = '#19bd00', mid = '#f5f671', high = '#fd0000', midpoint =6,
                         guide=guide_colourbar(title = "Percent"))
suppressWarnings(print(plt))
ggsave(filename = "Overall_Suicide_Attempt.png", path = "~/git/comm_fairfax/data/comm_fairfax/working/Youth_Survey_Heat_Maps/", device = "png")


#HEAT MAP for High Stress
plt <-ggplot(highSchool) +
    geom_polygon(data = highSchool.df, aes(x = long, y = lat, group = OBJECTID,
    fill = as.numeric(Stress_High)), color = "black") +
    labs(title = "% of Students reporting High Stress Levels") +
    scale_fill_gradient2(low = '#19bd00', mid = '#f5f671', high = '#fd0000', midpoint =40.57,
                         guide=guide_colourbar(title = "Percent"))
suppressWarnings(print(plt))
ggsave(filename = "Overall_High_Stress.png", path = "~/git/comm_fairfax/data/comm_fairfax/working/Youth_Survey_Heat_Maps/", device = "png")

#HEAT MAP for Binge Drinking
plt <-ggplot(highSchool) +
    geom_polygon(data = highSchool.df, aes(x = long, y = lat, group = OBJECTID,
                                           fill = as.numeric(Binge_Drinking)), color = "black") +
    labs(title = "% of Students reporting Binge Drinking") +
    scale_fill_gradient2(low = '#19bd00', mid = '#f5f671', high = '#fd0000', midpoint =8.6)
suppressWarnings(print(plt))
ggsave(filename = "Overall_Binge_Drinking.png", path = "~/git/comm_fairfax/data/comm_fairfax/working/Youth_Survey_Heat_Maps/", device = "png")

#HEAT MAP for Bullied Victims in school
plt <-ggplot(highSchool) +
    geom_polygon(data = highSchool.df, aes(x = long, y = lat, group = OBJECTID,
                                           fill = as.numeric(BulliedVic_School)), color = "black") +
    labs(title = "% of Students reporting Bullied in School") +
    scale_fill_gradient2(low = '#19bd00', mid = '#f5f671', high = '#fd0000', midpoint =11.6,
guide=guide_colourbar(title = "Percent"))
suppressWarnings(print(plt))
ggsave(filename = "Overall_BulliedVic_School.png", path = "~/git/comm_fairfax/data/comm_fairfax/working/Youth_Survey_Heat_Maps/", device = "png")

#HEAT MAP for Bullied Victims not in school
plt <-ggplot(highSchool) +
    geom_polygon(data = highSchool.df, aes(x = long, y = lat, group = OBJECTID,
                                           fill = as.numeric(BulliedVic_Not_School)), color = "black") +
    labs(title = "% of Students reporting Bullied Not in School") +
    scale_fill_gradient2(low = '#19bd00', mid = '#f5f671', high = '#fd0000', midpoint =9.33,
                         guide=guide_colourbar(title = "Percent"))
suppressWarnings(print(plt))
ggsave(filename = "Overall_BulliedVic_Not_School.png", path = "~/git/comm_fairfax/data/comm_fairfax/working/Youth_Survey_Heat_Maps/", device = "png")

#HEAT MAP for Cigarette Usage
plt <-ggplot(highSchool) +
    geom_polygon(data = highSchool.df, aes(x = long, y = lat, group = OBJECTID,
                                           fill = as.numeric(Cigarette_30)), color = "black") +
    labs(title = "% of Students reporting using Cigarette") +
    scale_fill_gradient2(low = '#19bd00', mid = '#f5f671', high = '#fd0000', midpoint =3.12,
                         guide=guide_colourbar(title = "Percent"))
suppressWarnings(print(plt))
ggsave(filename = "Overall_Cigarette_30.png", path = "~/git/comm_fairfax/data/comm_fairfax/working/Youth_Survey_Heat_Maps/", device = "png")

#HEAT MAP for Marijuana Usage
plt <-ggplot(highSchool) +
    geom_polygon(data = highSchool.df, aes(x = long, y = lat, group = OBJECTID,
                                           fill = as.numeric(Marijuana_30)), color = "black") +
    labs(title = "% of Students reporting using Marijuana") +
    scale_fill_gradient2(low = '#19bd00', mid = '#f5f671', high = '#fd0000', midpoint =9.17,
                         guide=guide_colourbar(title = "Percent"))
suppressWarnings(print(plt))
ggsave(filename = "Overall_Marijuana_30.png", path = "~/git/comm_fairfax/data/comm_fairfax/working/Youth_Survey_Heat_Maps/", device = "png")

#HEAT MAP for No Physical Activity
plt <-ggplot(highSchool) +
    geom_polygon(data = highSchool.df, aes(x = long, y = lat, group = OBJECTID,
                                           fill = as.numeric(Physical_Activity_None)), color = "black") +
    labs(title = "% of Students reporting No Physical Activity") +
    scale_fill_gradient2(low = '#19bd00', mid = '#f5f671', high = '#fd0000', midpoint =13.46,
                         guide=guide_colourbar(title = "Percent"))
suppressWarnings(print(plt))
ggsave(filename = "Overall_Physical_Activity_None.png", path = "~/git/comm_fairfax/data/comm_fairfax/working/Youth_Survey_Heat_Maps/", device = "png")

#HEAT MAP for Physical Activity Daily
plt <-ggplot(highSchool) +
    geom_polygon(data = highSchool.df, aes(x = long, y = lat, group = OBJECTID,
                                           fill = as.numeric(Physical_Activity_Daily)), color = "black") +
    labs(title = "% of Students reporting Physical Activity Daily") +
    scale_fill_gradient2(low = '#19bd00', mid = '#f5f671', high = '#fd0000', midpoint =19.31,
                         guide=guide_colourbar(title = "Percent"))
suppressWarnings(print(plt))
ggsave(filename = "Overall_Physical_Activity_Daily.png", path = "~/git/comm_fairfax/data/comm_fairfax/working/Youth_Survey_Heat_Maps/", device = "png")

#HEAT MAP for Extracurricular Available
plt <-ggplot(highSchool) +
    geom_polygon(data = highSchool.df, aes(x = long, y = lat, group = OBJECTID,
                                           fill = as.numeric(Extracurricular_Available)), color = "black") +
    labs(title = "% of Students reporting Extracurricular Available") +
    scale_fill_gradient2(low = '#19bd00', mid = '#f5f671', high = '#fd0000', midpoint =92.56,
                         guide=guide_colourbar(title = "Percent"))
suppressWarnings(print(plt))
ggsave(filename = "Overall_Extracurricular_Available.png", path = "~/git/comm_fairfax/data/comm_fairfax/working/Youth_Survey_Heat_Maps/", device = "png")

#HEAT MAP for Extracurricular Regularly
plt <-ggplot(highSchool) +
    geom_polygon(data = highSchool.df, aes(x = long, y = lat, group = OBJECTID,
                                           fill = as.numeric(Extracurricular_Regularly)), color = "black") +
    labs(title = "% of Students reporting Extracurricular Regularly") +
    scale_fill_gradient2(low = '#19bd00', mid = '#f5f671', high = '#fd0000', midpoint =72.06,
                         guide=guide_colourbar(title = "Percent"))
suppressWarnings(print(plt))
ggsave(filename = "Overall_Extracurricular_Regularly.png", path = "~/git/comm_fairfax/data/comm_fairfax/working/Youth_Survey_Heat_Maps/", device = "png")

#HEAT MAP for Frequency of Eating Fruits
plt <-ggplot(highSchool) +
    geom_polygon(data = highSchool.df, aes(x = long, y = lat, group = OBJECTID,
                                           fill = as.numeric(Fruit_Veg_5)), color = "black") +
    labs(title = "% of Students reporting Eating Fruits and Vegetables") +
    scale_fill_gradient2(low = '#19bd00', mid = '#f5f671', high = '#fd0000', midpoint =25.79,
                         guide=guide_colourbar(title = "Percent"))
suppressWarnings(print(plt))
ggsave(filename = "Overall_Fruit_Veg_5.png", path = "~/git/comm_fairfax/data/comm_fairfax/working/Youth_Survey_Heat_Maps/", device = "png")

#HEAT MAP for Cyberbullying School Victim
plt <-ggplot(highSchool) +
    geom_polygon(data = highSchool.df, aes(x = long, y = lat, group = OBJECTID,
                                           fill = as.numeric(Cyberbullying_SchoolVictim)), color = "black") +
    labs(title = "% of Students reporting in School Cyberbulling Victim") +
    scale_fill_gradient2(low = '#19bd00', mid = '#f5f671', high = '#fd0000', midpoint =11.26,
                         guide=guide_colourbar(title = "Percent"))
suppressWarnings(print(plt))
ggsave(filename = "Overall_Cyberbulling_SchoolVictim.png", path = "~/git/comm_fairfax/data/comm_fairfax/working/Youth_Survey_Heat_Maps/", device = "png")

#HEAT MAP for Cyberbullying School Aggressor
plt <-ggplot(highSchool) +
    geom_polygon(data = highSchool.df, aes(x = long, y = lat, group = OBJECTID,
                                           fill = as.numeric(Cyberbullying_SchoolAggressor)), color = "black") +
    labs(title = "% of Students reporting in School Cyberbulling Aggressor") +
    scale_fill_gradient2(low = '#19bd00', mid = '#f5f671', high = '#fd0000', midpoint =5.97,
                         guide=guide_colourbar(title = "Percent"))
suppressWarnings(print(plt))
ggsave(filename = "Overall_Cyberbulling_SchoolAggressor.png", path = "~/git/comm_fairfax/data/comm_fairfax/working/Youth_Survey_Heat_Maps/", device = "png")

#HEAT MAP for 4 or less hours of sleep
plt <-ggplot(highSchool) +
    geom_polygon(data = highSchool.df, aes(x = long, y = lat, group = OBJECTID,
                                           fill = as.numeric(`Sleep_4or less`)), color = "black") +
    labs(title = "% of Students reporting Sleeping 4 hours or less") +
    scale_fill_gradient2(low = '#19bd00', mid = '#f5f671', high = '#fd0000', midpoint =6.27,
                         guide=guide_colourbar(title = "Percent"))
suppressWarnings(print(plt))
ggsave(filename = "Overall_Sleep_4or_less.png", path = "~/git/comm_fairfax/data/comm_fairfax/working/Youth_Survey_Heat_Maps/", device = "png")

#HEAT MAP for Sleep 6 hours
plt <-ggplot(highSchool) +
    geom_polygon(data = highSchool.df, aes(x = long, y = lat, group = OBJECTID,
                                           fill = as.numeric(Sleep_6)), color = "black") +
    labs(title = "% of Students reporting Sleeping 6 hours") +
    scale_fill_gradient2(low = '#19bd00', mid = '#f5f671', high = '#fd0000', midpoint =25.69,
                         guide=guide_colourbar(title = "Percent"))
suppressWarnings(print(plt))
ggsave(filename = "Overall_Sleep_6.png", path = "~/git/comm_fairfax/data/comm_fairfax/working/Youth_Survey_Heat_Maps/", device = "png")

#HEAT MAP for Parent Help Available
plt <-ggplot(highSchool) +
    geom_polygon(data = highSchool.df, aes(x = long, y = lat, group = OBJECTID,
                                           fill = as.numeric(Parent_Help_Available)), color = "black") +
    labs(title = "% of Students reporting Parent Help Available") +
    scale_fill_gradient2(low = '#19bd00', mid = '#f5f671', high = '#fd0000', midpoint =79.75,
                         guide=guide_colourbar(title = "Percent"))
suppressWarnings(print(plt))
ggsave(filename = "Overall_Parent_Help_Available.png", path = "~/git/comm_fairfax/data/comm_fairfax/working/Youth_Survey_Heat_Maps/", device = "png")


#HEAT MAP for Adults Talk
plt <-ggplot(highSchool) +
    geom_polygon(data = highSchool.df, aes(x = long, y = lat, group = OBJECTID,
                                           fill = as.numeric(Adults_Talk)), color = "black") +
    labs(title = "% of Students reporting Adults Availablity to Talk") +
    scale_fill_gradient2(low = '#19bd00', mid = '#f5f671', high = '#fd0000', midpoint =39.79,
                         guide=guide_colourbar(title = "Percent"))
suppressWarnings(print(plt))
ggsave(filename = "Overall_Adults_Talking.png", path = "~/git/comm_fairfax/data/comm_fairfax/working/Youth_Survey_Heat_Maps/", device = "png")

#HEAT MAP for Gratitude
plt <-ggplot(highSchool) +
    geom_polygon(data = highSchool.df, aes(x = long, y = lat, group = OBJECTID,
                                           fill = as.numeric(Gratitude)), color = "black") +
    labs(title = "% of Students reporting Gratitude") +
    scale_fill_gradient2(low = '#19bd00', mid = '#f5f671', high = '#fd0000', midpoint =90.87,
                         guide=guide_colourbar(title = "Percent"))
suppressWarnings(print(plt))
ggsave(filename = "Overall_Gratitude.png", path = "~/git/comm_fairfax/data/comm_fairfax/working/Youth_Survey_Heat_Maps/", device = "png")

#HEAT MAP for Food Insecurity
plt <-ggplot(highSchool) +
    geom_polygon(data = highSchool.df, aes(x = long, y = lat, group = OBJECTID,
                                           fill = as.numeric(Food_Insecurity)), color = "black") +
    labs(title = "% of Students reporting Food Insecurity") +
    scale_fill_gradient2(low = '#19bd00', mid = '#f5f671', high = '#fd0000', midpoint =18.99,
                         guide=guide_colourbar(title = "Percent"))
suppressWarnings(print(plt))
ggsave(filename = "Overall_Food_Insecurity.png", path = "~/git/comm_fairfax/data/comm_fairfax/working/Youth_Survey_Heat_Maps/", device = "png")


##############FOR THE 8TH GRADERS
#JEFFERSON HIGH SCHOOL IS IN THE LOW TRIANGLE

#HEAT MAP for % of Students reporting Depressive Symptoms
plt <-ggplot(highSchool) +
    geom_polygon(data = highSchool.df8, aes(x = long, y = lat, group = OBJECTID,
                                           fill = as.numeric(Depressive_Symptoms)), color = "black") +
    labs(title = "% of Students reporting Depressive Symptoms") +
    scale_fill_gradient2(low = '#19bd00', mid = '#f5f671', high = '#fd0000', midpoint =22.02,
                         guide=guide_colourbar(title = "Percent"))
suppressWarnings(print(plt))
ggsave(filename = "8th_Grade_Depressive_Symptoms.png", path = "~/git/comm_fairfax/data/comm_fairfax/working/Youth_Survey_Heat_Maps/", device = "png")

#HEAT MAP for % of Students reporting Considering Suicide
plt <-ggplot(highSchool) +
    geom_polygon(data = highSchool.df8, aes(x = long, y = lat, group = OBJECTID,
                                           fill = as.numeric(Suicide_Consider)), color = "black") +
    labs(title = "% of Students reporting Considering Suicide") +
    scale_fill_gradient2(low = '#19bd00', mid = '#f5f671', high = '#fd0000', midpoint =10.78,
                         guide=guide_colourbar(title = "Percent"))
suppressWarnings(print(plt))
ggsave(filename = "8th_Grade_Considering_Suicide.png", path = "~/git/comm_fairfax/data/comm_fairfax/working/Youth_Survey_Heat_Maps/", device = "png")

#HEAT MAP for Suicide Attempt
plt <-ggplot(highSchool) +
    geom_polygon(data = highSchool.df8, aes(x = long, y = lat, group = OBJECTID,
                                           fill = as.numeric(Suicide_Attempt)), color = "black") +
    labs(title = "% of Students reporting Suicide Attempt") +
    scale_fill_gradient2(low = '#19bd00', mid = '#f5f671', high = '#fd0000', midpoint =5.64,
                         guide=guide_colourbar(title = "Percent"))
suppressWarnings(print(plt))
ggsave(filename = "8th_Grade_Suicide_Attempt.png", path = "~/git/comm_fairfax/data/comm_fairfax/working/Youth_Survey_Heat_Maps/", device = "png")

#HEAT MAP for High Stress
plt <-ggplot(highSchool) +
    geom_polygon(data = highSchool.df8, aes(x = long, y = lat, group = OBJECTID,
                                           fill = as.numeric(Stress_High)), color = "black") +
    labs(title = "% of Students reporting High Stress Levels") +
    scale_fill_gradient2(low = '#19bd00', mid = '#f5f671', high = '#fd0000', midpoint =20.82,
                         guide=guide_colourbar(title = "Percent"))
suppressWarnings(print(plt))
ggsave(filename = "8th_Grade_High_Stress.png", path = "~/git/comm_fairfax/data/comm_fairfax/working/Youth_Survey_Heat_Maps/", device = "png")





