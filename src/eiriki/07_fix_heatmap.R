#data exploration: heat maps and bar charts side by side
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
library(gridExtra)
library(grid)
library(ggrepel)

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
#Accessing the survey table
HS_Pyramid_Report <- read_excel("~/git/lab/comm_fairfax/data/comm_fairfax/original/2015 Supplemental Analysis by Pyramid Report__GIS.xlsx",
                                sheet = "8-10-12 Results by Pyramid")

#Making the new data table for the specific catergories
HS_Pyramid_Report1 <- HS_Pyramid_Report[,c(1,2,3,
                                           90,91,92,
                                           93,94,95,
                                           105,72,73,101,106,134,135,136,
                                           20,21,126,
                                           88,89,138,140,67,48,52,61)]
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

#make all columns numerics
HS_Pyramid_Report1_mh_overall[,4:27] <- sapply(HS_Pyramid_Report1_mh_overall[4:27], as.numeric)
HS_Pyramid_Report1_mh_overall[,4:27] <- round(HS_Pyramid_Report1_mh_overall[,4:27], digits = 2)

#for convenience below
youth_results_mh_overall <- HS_Pyramid_Report1_mh_overall

#joining the survey results to the highSchool map data for each grade level
highSchool_percent_count <- youth_results_mh_overall %>% left_join(highSchool@data, by = c("Pyramid" = "SCHOOL_NAM"))

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

#make the labels for fairfax, lee, and mount vernon
centroids.df <- as.data.frame(coordinates(highSchool))
names(centroids.df) <- c("Longitude", "Latitude")
centroids.df$id <- highSchool$SCHOOL_NAM
centroids.df <- left_join(centroids.df, highSchool_percent_count, by =c("id"= "Pyramid"))

#EXAMPLE HEATMAP WITH BAR CHART IN DECREASING ORDER
#Overall heatmap for Depressive symptoms
#make a bar chart that is ordered from high to low depressive symptoms
bchart <- ggplot(youth_results_mh_overall, aes(x = reorder(Pyramid, -as.numeric(Depressive_Symptoms)),
                                               y= Depressive_Symptoms, fill = as.numeric(Depressive_Symptoms))) +
    geom_bar(stat = 'identity')+
    scale_fill_gradient2(low = '#19bd00', mid = '#f5f671', high = '#fd0000', midpoint = 26)+
    guides(fill = FALSE) +
    labs( x= 'High School Pyramid') +
    theme(axis.text.x=element_text(size=15,angle=45,hjust=1,vjust=1), axis.text.y = element_text(size = 15),
          axis.title.x = element_text(size = 15),axis.title.y = element_text(size = 15)) +
    scale_y_continuous(name="Percent", breaks = seq(20,32,1),limits=c(20, 32),oob = rescale_none)
#make the same heatmap as before but hide all elements so it's legible
plt <-ggplot(highSchool) +
    geom_polygon(data = highSchool.df, aes(x = long, y = lat, group = OBJECTID,
                                           fill = as.numeric(Depressive_Symptoms)), color = "black") +
    scale_fill_gradient2(low = '#19bd00', mid = '#f5f671', high = '#fd0000', midpoint = 26,
                         guide = guide_colourbar(title = "Percent")) +
    theme(axis.line=element_blank(),axis.text.x=element_blank(),
            axis.text.y=element_blank(),axis.ticks=element_blank(),
            axis.title.x=element_blank(), legend.key.size = unit(.5, 'in'), legend.text = element_text(size = 15),
            legend.position = ('left'),
            axis.title.y=element_blank(),
            panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),plot.background=element_blank())
    #geom_text_repel(data = centroids.df, aes(x = Longitude, y = Latitude, label = paste(id,round(Depressive_Symptoms,digits=0),sep='\n'))
     #                , color = "black", size = 4, nudge_x = -.005, nudge_y = -.0025)
both1 <- grid.arrange(plt,bchart, ncol = 2, top = textGrob('% of Overall Students reporting Depressive Symptoms', gp=gpar(fontsize=20)))
#use this to save ggsave(both1, filename = "over_depress_with_bar.png",
#path = "~/git/lab/comm_fairfax/data/comm_fairfax/working/Youth_Survey_Heat_Maps/with_bar_chart",
#device = "png", width=20,height=11.25,scale=1)

#FOOD INSECURITY
bchart2 <- ggplot(youth_results_mh_overall, aes(x = reorder(Pyramid, -as.numeric(Food_Insecurity)),
                                               y= Food_Insecurity, fill = as.numeric(Food_Insecurity))) +
    geom_bar(stat = 'identity')+
    scale_fill_gradient2(low = '#19bd00', mid = '#f5f671', high = '#fd0000', midpoint = 18.99)+
    labs( x= 'High School Pyramid') +
    theme(axis.text.x=element_text(size=15,angle=45,hjust=1,vjust=1), axis.text.y = element_text(size = 15),
          axis.title.x = element_text(size = 15),axis.title.y = element_text(size = 15)) +
    scale_y_continuous(name="Percent", breaks = seq(5,33,1),limits=c(5, 33),oob = rescale_none)+
    guides(fill = FALSE)
plt2 <-ggplot(highSchool) +
    geom_polygon(data = highSchool.df, aes(x = long, y = lat, group = OBJECTID,
                                           fill = as.numeric(Food_Insecurity)), color = "black") +
    scale_fill_gradient2(low = '#19bd00', mid = '#f5f671', high = '#fd0000', midpoint = 18.99,
                         guide = guide_colourbar(title = "Percent")) +
    theme(axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(), legend.key.size = unit(.5, 'in'), legend.text = element_text(size = 15),
          legend.position = ('left'),
          axis.title.y=element_blank(),
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank())
    #geom_text(data = centroids.df, aes(x = Longitude, y = Latitude, label = id), color = "blue4", size = 5)
both2 <- grid.arrange(plt2,bchart2, ncol = 2, top = textGrob('% of Overall Students reporting Food Insecurity', gp=gpar(fontsize=20)))

#Physical Activity None
bchart3 <- ggplot(youth_results_mh_overall, aes(x = reorder(Pyramid, -as.numeric(Physical_Activity_None)),
                                                y= Physical_Activity_None, fill = as.numeric(Physical_Activity_None))) +
    geom_bar(stat = 'identity')+
    scale_fill_gradient2(low = '#19bd00', mid = '#f5f671', high = '#fd0000', midpoint = 13.46)+
    guides(fill = FALSE)+
    labs( x= 'High School Pyramid') +
    theme(axis.text.x=element_text(size=15,angle=45,hjust=1,vjust=1), axis.text.y = element_text(size = 15),
          axis.title.x = element_text(size = 15),axis.title.y = element_text(size = 15)) +
    scale_y_continuous(name="Percent", breaks = seq(8,19,1),limits=c(8, 19),oob = rescale_none)
plt3 <-ggplot(highSchool) +
    geom_polygon(data = highSchool.df, aes(x =long, y = lat, group = OBJECTID,
                                           fill = as.numeric(Physical_Activity_None)), color = "black") +
    scale_fill_gradient2(low = '#19bd00', mid = '#f5f671', high = '#fd0000', midpoint = 13.46,
                         guide = guide_colourbar(title = "Percent")) +
    theme(axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(), legend.key.size = unit(.5, 'in'), legend.text = element_text(size = 15),
          legend.position = ('left'),
          axis.title.y=element_blank(),
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank())
    #geom_text(data = centroids.df, aes(x = Longitude, y = Latitude, label = id), color = "blue4", size = 5)
both3 <- grid.arrange(plt3,bchart3, ncol = 2, top = textGrob('% of Overall Students reporting No daily physical activity',gp=gpar(fontsize=20)))

#Parent Help Available
bchart4 <- ggplot(youth_results_mh_overall, aes(x = reorder(Pyramid, -as.numeric(Parent_Help_Available)),
                                                y= Parent_Help_Available, fill = as.numeric(Parent_Help_Available))) +
    geom_bar(stat = 'identity')+
    scale_fill_gradient2(low = '#fd0000', mid = '#f5f671', high = '#19bd00', midpoint = 79.75)+
    labs( x= 'High School Pyramid') +
    theme(axis.text.x=element_text(size=15,angle=45,hjust=1,vjust=1), axis.text.y = element_text(size = 15),
          axis.title.x = element_text(size = 15),axis.title.y = element_text(size = 15)) +
    scale_y_continuous(name="Percent", breaks = seq(73,87,1),limits=c(73, 87),oob = rescale_none)+
    guides(fill = FALSE)
plt4 <-ggplot(highSchool) +
    geom_polygon(data = highSchool.df, aes(x = long, y = lat, group = OBJECTID,
                                           fill = as.numeric(Parent_Help_Available)), color = "black") +
    scale_fill_gradient2(low = '#fd0000', mid = '#f5f671', high = '#19bd00', midpoint = 79.75,
                         guide = guide_colourbar(title = "Percent")) +
    theme(axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(), legend.key.size = unit(.5, 'in'), legend.text = element_text(size = 15),
          legend.position = ('left'),
          axis.title.y=element_blank(),
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank())
    #geom_text(data = centroids.df, aes(x = Longitude, y = Latitude, label = id), color = "blue4", size = 5)
both4 <- grid.arrange(plt4,bchart4, ncol = 2, top = textGrob('% of Overall Students reporting Parent Help',gp=gpar(fontsize=20)))

#Extracurricular Regularly
bchart5 <- ggplot(youth_results_mh_overall, aes(x = reorder(Pyramid, -as.numeric(Extracurricular_Regularly)),
                                                y= Extracurricular_Regularly, fill = as.numeric(Extracurricular_Regularly))) +
    geom_bar(stat = 'identity')+
    scale_fill_gradient2(low = '#fd0000', mid = '#f5f671', high = '#19bd00', midpoint = 72.06)+
    guides(fill =FALSE)+
    labs( x= 'High School Pyramid') +
    theme(axis.text.x=element_text(size=15,angle=45,hjust=1,vjust=1), axis.text.y = element_text(size = 15),
          axis.title.x = element_text(size = 15),axis.title.y = element_text(size = 15)) +
    scale_y_continuous(name="Percent", breaks = seq(58,87,1),limits=c(58, 87),oob = rescale_none)
plt5 <-ggplot(highSchool) +
    geom_polygon(data = highSchool.df, aes(x = long, y = lat, group = OBJECTID,
                                           fill = as.numeric(Extracurricular_Regularly)), color = "black") +
    scale_fill_gradient2(low = '#fd0000', mid = '#f5f671', high = '#19bd00', midpoint = 72.06,
                         guide = guide_colourbar(title = "Percent")) +
    theme(axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(), legend.key.size = unit(.5, 'in'), legend.text = element_text(size = 15),
          legend.position = ('left'),
          axis.title.y=element_blank(),
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank())
    #geom_text(data = centroids.df, aes(x = Longitude, y = Latitude, label = id), color = "blue4", size = 5)
both5 <- grid.arrange(plt5,bchart5, ncol = 2, top = textGrob('% of Overall Students reporting Extracurricular Regularly',gp=gpar(fontsize=20)))
