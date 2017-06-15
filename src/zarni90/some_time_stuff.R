#The file for zipcodes is here, but I didn't use it in the code... ("~/sdal/projects/arlington911/data/original/gis/Arlington_Zip_Codes/cb_2015_us_zcta510_500k.shp")

library(maptools)
library(ggplot2)

arlington <- readShapePoly("~/sdal/projects/arlington911/data/original/gis/Arlington_County_Polygon/County_Polygon.shp")
arlington <- arlington[arlington@data$ZCTA5CE10 %in% c("22101", "22201", "22202", "22203", "22204", "22205", "22206", "22207", "22209", "22211", "22213", "22214"), ]

beats <- read.csv("~/sdal/projects/arlington911/data/working/police/BeatShapefile_Edited.csv")

ggplot(data = arlington) +
    geom_polygon(mapping = aes(x = long, y = lat, group = group), fill = NA, color = "black") +
    geom_polygon(data = beats %>% filter(beat_num < 20), mapping = aes(x = long, y = lat, group = group), fill = NA, color = "blue")


#Ye Oo Zin Time Interval Problem
library(lubridate)
s.times <- c("10:00:00")
s.times.pos <- as.POSIXct(strptime(s.times, "%H:%M:%S"))
s.times.pos

t.times <- c("10:10:00")
t.times.pos <- as.POSIXct(strptime(t.times,"%H:%M:%S"))
t.times.pos

as.difftime(t.times.pos, s.times.pos, format = "%H%M%S")
#as.difftime)(difftime(t.times.pos, s.times.pos, units = "mins")


interval.in <- as.difftime(c("10:00:00", "10:10:00"), format = "%H%M%S")
interval <-  seq.POSIXt(s.times.pos, by = z, length.out = 10)

#Time Series but, it has dates
interval
#http://stackoverflow.com/questions/15982722/strip-the-date-and-keep-the-time
