
if (!require(rgeos)) {
  install.packages("rgeos", repos = "http://cran.us.r-project.org")
  require(rgeos)
}
if (!require(rgdal)) {
  install.packages("rgdal", repos = "http://cran.us.r-project.org")
  require(rgdal)
}
if (!require(raster)) {
  install.packages("raster", repos = "http://cran.us.r-project.org")
  require(raster)
}
if(!require(ggplot2)) {
  install.packages("ggplot2", repos="http://cloud.r-project.org")
  require(ggplot2)
}
if(!require(viridis)) {
  install.packages("viridis", repos="http://cloud.r-project.org")
  require(viridis)
}
if(!require(dplyr)) {
  install.packages("dplyr", repos = "https://cloud.r-project.org/")
  require(dplyr)
}
if(!require(gtable)) {
  install.packages("gtable", repos = "https://cloud.r-project.org/")
  require(gtable)
}
if(!require(grid)) {
  install.packages("grid", repos = "https://cloud.r-project.org/")
  require(grid)
}
if(!require(readxl)) {
  install.packages("readxl", repos = "https://cloud.r-project.org/")
  require(readxl)
}
if(!require(magrittr)) {
  install.packages("magrittr", repos = "https://cloud.r-project.org/")
  require(magrittr)
}
if (!require(extrafont)) {
  install.packages("extrafont", repos = "http://cran.us.r-project.org")
  require(extrafont)
}
if (!require(tigris)) {
  install.packages("tigris", repos = "http://cran.us.r-project.org")
  require(tigris)
}
if (!require(sp)) {
  install.packages("sp", repos = "http://cran.us.r-project.org")
  require(sp)
}
if (!require(data.table)) {
  install.packages("data.table", repos = "http://cran.us.r-project.org")
  require(data.table)
}
library(dplyr)
# Get plots Vickified

theme_map <- function(...) {
    theme_minimal() +
        theme(
            text=element_text(family="sans", color="#22211d"),
            axis.line=element_blank(),
            axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            panel.grid.major=element_blank(),
            #panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
            panel.grid.minor=element_blank(),
            #panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
            plot.background=element_rect(fill="#f5f5f2", color = NA),
            panel.background=element_rect(fill="#f5f5f2", color = NA),
            legend.background=element_rect(fill="#f5f5f2", color = NA),
            panel.border=element_blank(),
            ...
        )
}


#Get rid of exponential notation
options(scipen=999)

#Read in Data
SNAP<-read_excel("./data/comm_fairfax/Retail Data/SNAP Retailers.xls")
View(SNAP)

# Read pyramid boundary shape file

pyramids <- readOGR('data/comm_fairfax/working/formatted_youth_survey/High_School_Pyramids',
                    "High_School_Attendance_Areas")
pyramids@data = dplyr::select(pyramids@data, one_of("OBJECTID", "SCHOOL_NAM"))
pyramids@data$id <- rownames(pyramids@data)
pyramids.points <- fortify(pyramids)

# Extract the coordinates from the SNAP file and set it to some kind of polygon object.

snapCoords = data.frame(long = SNAP$Long, lat = SNAP$Lat)
coordinates(snapCoords) <- ~ long + lat

# Set the projection of the SpatialPointsDataFrame using the projection of the shapefile
proj4string(snapCoords) <- proj4string(pyramids)

# Each row of this corresponds to a row in the SNAP file. It give the pyramid data corresponding to that coordinate.
over(snapCoords, pyramids)

snapWithPyramid = data.frame(isGrocery = ifelse(SNAP$NAIC == 445110, 1, 0), over(snapCoords, pyramids))

snapWithPyramid %>%
    group_by(id) %>%
    summarize(propHealthy = sum(isGrocery)/n()) ->
    propHealthyPerPyramid
propHealthyPerPyramid[25, 1] = 24

# Write table of name, id, and proportion of healthy snap retailers to file
write.csv(merge(pyramids@data[,-1], propHealthyPerPyramid, all.x = T) %>% arrange(as.numeric(id)), "./data/comm_fairfax/pyramid level data/propHealthySNAP.csv", row.names = F)


pyramids.points %>% merge(propHealthyPerPyramid) %>%
    arrange(group, order) ->
    pyramidData

ggplot() +
    geom_polygon(data = pyramidData, aes(x = long, y = lat, group = group, fill = propHealthy)) +
    geom_polygon(data = pyramidData, aes(x=long, y=lat, group=group),
                 colour="grey70", linetype=1, size=0.5, fill=NA) +
    geom_point(data = data.frame(snapCoords)[snapWithPyramid$isGrocery == 1,], aes(x = long, y = lat), shape=21, colour="black", fill="white", size=2) +
    theme_map() +
    labs(x=NULL,
         y=NULL,
         title="Fairfax County Percentage of SNAP Grocery Store Retailers by HS Pyramid \n(Points identify SNAP grocery stores, NAIC Code 445110).",
         subtitle="Data: USDA Food and Nutrition SNAP Retailers, 2017; Simply Analytics Business Data, 2017",
         caption = "Geometry: Fairfax County High School Pyramid Boundaries") +
    theme(legend.position = "bottom") +
    scale_fill_viridis(
        option = "viridis",
        direction = -1,
        name = "Percentage",
        guide = guide_colorbar(
            ticks = F,
            nbins=100,
            direction = "horizontal",
            barheight = unit(3, units = "mm"),
            barwidth = unit(100, units = "mm"),
            title.position = "top",
            title.hjust = 0.5,
            label.hjust = 1,
            nrow = 1,
            byrow = T,
            label.position = "bottom"
        )
    )

