library(ggmap)

# load project data (police data, for example)
data <- read.csv("~/sdal/projects/limbo/temp_inmast_clean.csv")

# only keep Arlington regions/beats (remove Fairfax, DC, etc.)
goodbeats <- c(10:12,23:24,35:37,48:49)
data <- data %>% filter(Beat %in% goodbeats)

# only need region column
data_beat <- data$Beat

# get the counts by region
aggregated_beats <- aggregate(data_beat,
                              by = list(data_beat),
                              FUN = "NROW")
aggregated_beats

# reassign column names so that column 1 matches the column name in your *edited* shapefile
colnames(aggregated_beats) <- c("beat_num", "count")

# load edited shapefile
beat_polygon <- read.csv("~/sdal/projects/arlington911/data/working/police/BeatShapefile_Edited.csv")
names(beat_polygon)

# I used this map to assign beat_num manually (below) by comparing this map with a map of Arlington police beats
ggplot(beat_polygon) +
    geom_polygon(aes(x = long, y = lat, group = group, fill = as.character(id)), color = "black") + coord_map()

# assigning beat_num so the beat codes align with both the shapefile data AND the police data
# beat_polygon$beat_num[beat_polygon$id == "0"] <- 11
# beat_polygon$beat_num[beat_polygon$id == "1"] <- 12
# beat_polygon$beat_num[beat_polygon$id == "2"] <- 35
# beat_polygon$beat_num[beat_polygon$id == "3"] <- 36
# beat_polygon$beat_num[beat_polygon$id == "4"] <- 37
# etc, etc...

# combine shapefile with aggregated project data
map_data <- merge(x = beat_polygon, y = aggregated_beats, by="beat_num", all.x=TRUE)

ArlingtonMap <- ggmap::qmap("arlington county va", zoom = 12, maptype="roadmap", legend = "topright")

ArlingtonMap +
    geom_polygon(data= map_data,
                 aes(x = long, y = lat, group = group, fill = count), color = "black")
