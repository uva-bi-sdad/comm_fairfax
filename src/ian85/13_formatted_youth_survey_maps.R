# I turned 2015 Supplemental Analysis by Pyramid Report__GIS into .csv
# Put them in the folder path data/comm_fairfax/working/formatted_youth_survey/

# Copied over GIS shapefiles from limbo workspace into:
# data/comm_fairfax/working/formatted_youth_survey/

library(rgdal)
library(ggmap)
library(raster)
library(dplyr)
library(viridis)
library(ggmap)
library(gridExtra)
library(ggrepel)
library(grid)

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


sixth <- read.csv('data/comm_fairfax/working/formatted_youth_survey/2015_supplemental_6_pyramid.csv')
older <- read.csv('data/comm_fairfax/working/formatted_youth_survey/2015_supplemental_8_10_12_pyramid.csv')

pyramids <- readOGR('data/comm_fairfax/working/formatted_youth_survey/High_School_Pyramids',
                    "High_School_Attendance_Areas")

#joining the survey results to the pyramids map data for all grade levels

pyramidsPercentCount <- subset(older, Demographic == "Overall") %>% left_join(pyramids@data[,1:2], by = c("Pyramid" = "SCHOOL_NAM"))

#Convert Polygon information into data frame is what fortify does
pyramids@data$id <- rownames(pyramids@data)
pyramids.points <- fortify(pyramids)



pyramids.points %>%
    left_join(select(pyramids@data, one_of("id", "OBJECTID")), by = "id") %>%
    left_join(pyramidsPercentCount, by = "OBJECTID") ->
    pyramids.df

rm(pyramids.points)

# Load in the locations of the high schools themselves.

hsLocations = read.csv("./data/comm_fairfax/pyramid level data/ffxHsLatLong.csv")

# Make vectors giving the columns for exercise related things and for nutrition related things

exercise.cols = c("Physical_Activity_None", "Physical_Activity_5Plus")
food.cols = c("Fruit_Veg_5", "Combined_sugary_drinks")
weight.cols = "Unhealthy_Weight_Loss"
all.cols = c(exercise.cols, food.cols, weight.cols, "Food_Insecurity")
plot.list = vector("list", length(all.cols))
titles = c("No Physical Activity in the Last Week", "Five or More Days of Physical Activity", "At least 5 Servings of Fruit or Veg", "At Least One Sugary Drink Per Day", "Unhealthy Weight Loss Activities", "Food Insecurity")

for(i in 1:length(all.cols)){
    plot.list[[i]] =
        ggplot() + geom_polygon(data = pyramids.df, aes_string(x = "long", y = "lat", group = "group", fill = all.cols[i]), color = "black") +
        geom_point(data = data.frame(hsLocations), aes(x = long, y = lat), shape = 21, colour = "black", fill = "white", size = 2) +
        labs(title = all.cols[i]) +
        theme_map() +
        labs(x=NULL,
             y=NULL,
             title= titles[i],
             subtitle="Data: 2015 Fairfax Youth Survey; HS locations from Google maps.",
             caption = "Geometry: Fairfax County High School Pyramid Boundaries, Points Denote High Schools") +
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
            ))

}

plot.out.dir = "./output/ffxysMaps/"
par(mfrow = c(2, 3))
for(i in 1:length(all.cols)){
    #pdf(paste0(plot.out.dir, all.cols[i], ".pdf"))
    plot(plot.list[[i]])
   # dev.off()
}


cor(select(older, all.cols))

# Obesity risk index, pr1 of response, pr from PLS?
#
# Make a display of the response variable maps (including food insecurity now) alongside a pairs plot (all pairwise scatterplots). Maybe get correlation in there?
# For each response, make a paired map/barplot.  Eirik's code.

# Maps side by side with barcharts
#HEATMAPS WITH BAR CHART IN DECREASING ORDER

ffx_map <- get_map(location = c(long = -77.269848, lat = 38.836325),zoom = 10, color = 'bw', source = "google")

mapHistList = vector("list", length(all.cols))

for(i in 1:length(all.cols)){
    bchart <-
        ggplot(pyramidsPercentCount, aes(x = reorder(Pyramid, -get(all.cols[i])),  y = get(all.cols[i]), fill = get(all.cols[i]))) +
        geom_bar(stat = 'identity')+
        scale_fill_viridis(direction = -1) +
        guides(fill = FALSE) +
        labs(x = 'High School Pyramid') +
        theme(axis.text.x=element_text(size=23,angle=45,hjust=1,vjust=1),
              axis.text.y = element_text(size = 23),
              axis.title.x = element_text(size = 23),
              axis.title.y = element_text(size = 23),
              plot.background=element_rect(fill="#f5f5f2", color = NA),
              panel.background=element_rect(fill="#f5f5f2", color = NA),
              legend.background=element_rect(fill="#f5f5f2", color = NA),
              plot.margin=unit(c(1,1,1,2), "cm")) +
        scale_y_continuous(name="Percent")

    plt = ggmap(ffx_map) +
        coord_fixed(ratio = .7) +
        coord_cartesian(xlim=range(pyramids.df$long) + c(-.01, .01), ylim=range(pyramids.df$lat) + c(-.01, .01)) +
        geom_polygon(data = pyramids.df, aes(x = long, y = lat, group = group, fill = get(all.cols[i])), color = "black", alpha = .9, size = .4) +
        geom_point(data = data.frame(hsLocations), aes(x = long, y = lat), shape = 21, colour = "black", fill = "white", size = 2) +
        labs(title = titles[i]) +
        theme_map() +
        labs(x=NULL,
             y=NULL,
             subtitle="Data: 2015 Fairfax Youth Survey; HS locations from Google maps.",
             caption = "Geometry: Fairfax County High School Pyramid Boundaries, Points Denote High Schools") +
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
            ))

    mapHistList[[i]] =grid.arrange(plt, bchart, ncol = 2)
}



for(i in 1:length(all.cols)){
    #pdf(paste0(plot.out.dir, all.cols[i], "_mapHist.pdf"), height = 10, width = 22)
    plot(mapHistList[[i]])
    #dev.off()
}

pairs(scale(select(older, all.cols)), col = viridis(25), pch = 15, cex.labels = 2)


cor(select(older, all.cols))




