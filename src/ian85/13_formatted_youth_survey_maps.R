# I turned 2015 Supplemental Analysis by Pyramid Report__GIS into .csv
# Put them in the folder path data/comm_fairfax/working/formatted_youth_survey/

# Copied over GIS shapefiles from limbo workspace into:
# data/comm_fairfax/working/formatted_youth_survey/

library(rgdal)
library(ggmap)
library(raster)
library(dplyr)
library(viridis)

sixth <- read.csv('data/comm_fairfax/working/formatted_youth_survey/2015_supplemental_6_pyramid.csv')
older <- read.csv('data/comm_fairfax/working/formatted_youth_survey/2015_supplemental_8_10_12_pyramid.csv')

pyramids <- readOGR('data/comm_fairfax/working/formatted_youth_survey/High_School_Pyramids',
                    "High_School_Attendance_Areas")

#joining the survey results to the pyramids map data for all grade levels

pyramidsPercentCount <- subset(older, Demographic == "Overall") %>% left_join(pyramids@data[,1:2], by = c("Pyramid" = "SCHOOL_NAM"))

#Convert Polygon information into data frame is what fortify does
pyramids@data$id <- rownames(pyramids@data)
pyramids.points <- fortify(pyramids)
ggplot() + geom_polygon(data = pyramids.points, aes_string(x = "long", y = "lat", group = "group"), color = "black")

#we are joining the Lat Long data with our shape data that we joined in line 71
#do this three times: once for each grade level

pyramids.points %>%
    left_join(select(pyramids@data, one_of("id", "OBJECTID")), by = "id") %>%
    left_join(pyramidsPercentCount, by = "OBJECTID") ->
    pyramids.df

rm(pyramidsPercentCount, pyramids, pyramids.points)

# Make vectors giving the columns for exercise related things and for nutrition related things

exercise.cols = c("Physical_Activity_None", "Physical_Activity_5Plus")
food.cols = c("Fruit_Veg_5", "Combined_sugary_drinks")
weight.cols = "Unhealthy_Weight_Loss"
all.cols = c(exercise.cols, food.cols, weight.cols)
plot.list = vector("list", length(all.cols))

for(i in 1:length(all.cols)){
    plot.list[[i]] = ggplot() + geom_polygon(data = pyramids.df, aes_string(x = "long", y = "lat", group = "group", fill = all.cols[i]), color = "black") +
        labs(title = all.cols[i]) +
        scale_fill_viridis(guide = guide_colourbar(title = "Percent"))

}
plot(plot.list[[5]])











