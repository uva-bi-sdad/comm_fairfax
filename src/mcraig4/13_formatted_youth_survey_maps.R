# I turned 2015 Supplemental Analysis by Pyramid Report__GIS into .csv
# Put them in the folder path data/comm_fairfax/working/formatted_youth_survey/

# Copied over GIS shapefiles from limbo workspace into:
# data/comm_fairfax/working/formatted_youth_survey/

library(rgdal)
library(ggmap)
library(raster)
library(dplyr)

sixth <- read.csv('data/comm_fairfax/working/formatted_youth_survey/2015_supplemental_6_pyramid.csv')
older <- read.csv('data/comm_fairfax/working/formatted_youth_survey/2015_supplemental_8_10_12_pyramid.csv')

pyramids <- readOGR('data/comm_fairfax/working/formatted_youth_survey/High_School_Pyramids',
                    "High_School_Attendance_Areas")

# Look back at Eirik's code to join the .csv with the shapefile
# Name of HS is in pyramids
# How do you get this shapefile into working map_data?

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


# Convert Everything to the coordinate system for the raster map
pyramids <- spTransform(pyramids, rgmaprgb@crs)
plot(pyramids)
#~~~~~~~~~~ end of copying zarni's code for GIS stuff

# Make different data sets depending on grade level (chanida will do 8)
# Can also break up older between male and female
youth_results_overall <- subset(older, Demographic == "Overall")
youth_results_8 <- subset(older, Demographic == "8th Grade")
youth_results_10 <- subset(older, Demographic == "10th Grade")
youth_results_12 <- subset(older, Demographic == "12th Grade")

#joining the survey results to the pyramids map data for each grade level
pyramids_percent_count <- youth_results_overall %>% left_join(pyramids@data, by = c("Pyramid" = "SCHOOL_NAM"))
pyramids_percent_count_10 <- youth_results_10 %>% left_join(pyramids@data, by = c("Pyramid" = "SCHOOL_NAM"))
pyramids_percent_count_12 <- youth_results_12 %>% left_join(pyramids@data, by = c("Pyramid" = "SCHOOL_NAM"))

#~~~~~~~~~Based on Bianica's Code
names(pyramids@data)
pyramids@data$id <- rownames(pyramids@data)
pyramids@data$id

#Convert Polygon information into data frame is what fortify does
pyramids.points <- fortify(pyramids)
names(pyramids.points)

#we are joining the Lat Long data with our shape data that we joined in line 71
#do this three times: once for each grade level
pyramids.df <- left_join(pyramids.points, pyramids@data, by = "id")
pyramids.df <- left_join(pyramids.df, pyramids_percent_count, by = "OBJECTID")

pyramids.df_10 <- left_join(pyramids.points, pyramids@data, by = "id")
pyramids.df_10 <- left_join(pyramids.df_10, pyramids_percent_count_10, by = "OBJECTID")

pyramids.df_12 <- left_join(pyramids.points, pyramids@data, by = "id")
pyramids.df_12 <- left_join(pyramids.df_12, pyramids_percent_count_12, by = "OBJECTID")

#EXAMPLE HEATMAP
#Overall heatmap for Depressive symptoms
plt <-ggplot(pyramids) +
    geom_polygon(data = pyramids.df, aes(x = long, y = lat, group = OBJECTID,
                                           fill = as.numeric(TV_3Plus)), color = "black") +
    labs(title = "% of Students watching more than 3 hours of TV") +
    scale_fill_gradient2(low = "#F0E442", high = "#D55E00",
                         guide = guide_colourbar(title = "Percent"))
suppressWarnings(print(plt))
