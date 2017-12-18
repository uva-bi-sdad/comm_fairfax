#
# This code reads, maps, and analyzes data from the USDA food environment atlas.
#

library(ggplot2)
library(maps)
library(dplyr)
library(glmnet)
library(randomForest)
library(ggthemes)
library(viridis)
theme_set(theme_fivethirtyeight())

# Insert Vicki's map format function
theme_map <- function(...) {
    theme_minimal() +
        theme(
            text=element_text(family="sans", color="#22211d"),
            plot.title = element_text(size=20),
            plot.subtitle = element_text(size=17),
            plot.caption = element_text(size=17),
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

# Set the directory, load the fea table names, and start a scheme to index the various files for later looping.

data.dir = "./data/comm_fairfax/working/usda_data/"
fea.tables = list.files(data.dir, pattern = "FEA")

table.index = 1


# Some preprocessing is needed to make sure the fea data merges nicely with the map data.

clean.maps.counties = function(){

    # Minor changes to get counties in working form
    counties <- map_data("county")
    counties$subregion = gsub('\\.','',counties$subregion)
    counties$subregion = gsub("'",'', counties$subregion)
    counties$subregion = gsub("\\s",'', counties$subregion)
    return(counties)
}
clean.state.fips = function(){
    data("state.fips")
    # Minor changes to get state.fips into usable form
    state.fips$polyname[20:22] = "massachusetts"
    state.fips$polyname[23:24] = "michigan"
    state.fips$polyname[34:37] = "new york"
    state.fips$polyname[38:40] = "north carolina"
    state.fips$polyname[53:55] = "virginia"
    state.fips$polyname[56:60] = "washington"
    return(state.fips)
}
read.clean.fea.data = function(data.dir, fea.tables, index){
    fea.data = read.csv(paste0(data.dir, fea.tables[index]), stringsAsFactors = F)

    # Minor changes to get county_obesity into usable form
    fea.data$County[which(fea.data$County == "Doña Ana")] = "dona ana"
    fea.data$County = tolower(fea.data$County)
    # Have to get rid of the . behind counties with the name like St. Mary's
    fea.data$County = gsub('\\.','',fea.data$County)
    fea.data$County = gsub("'",'', fea.data$County)
    fea.data$County = gsub("\\s",'', fea.data$County)
    return(fea.data)
}
merge.maps.and.data = function(fea.data){
    counties = clean.maps.counties()
    state.fips = clean.state.fips()

    # Join counties and state.fips
    # Need the state abbreviation from the stat.fips frame
    counties_st_ab <- left_join(counties, state.fips, by = c("region" = "polyname"))

    # Merge on two variables so counties with the same name correspond to the right state
    # Got line 54 from looking through ?map_data
    fea.map.data <- merge(counties_st_ab, fea.data, by.x = c("subregion", "abb"), by.y = c("County", "State"))
    fea.map.data <- fea.map.data[order(fea.map.data$order), ]
    return(fea.map.data)
}

# This will plot the map data with some predefined graphical parameters. As we develop the code, we'll probably end up adding more arguments to the function to allow us finer control over the output.

plot.usda = function(data, colname, state, plot = T, title){

    limits = range(data[,colname], na.rm = T)
    breaks = round(seq(min(data[,colname], na.rm = T), max(data[,colname], na.rm = T), length = 5), 1)
    if(missing(title)) title = colname
    if(!missing(state)) data = subset(data, data$abb == state)

    usda.map = ggplot() +
        geom_polygon(data=data, aes(x=long, y=lat, group=group, fill=data[,colname]), color="black", size=.25) +
        geom_polygon(data=data, aes(x=long, y=lat, group=group), fill=NA, color="gray48", size=.25) +
        coord_fixed(1.3) +
        labs(x=NULL,
             y=NULL,
             title= "Price of Soda to National Average, 2010",
             subtitle="Data: 2016 USDA Economic Research Service",
             caption = "Geometry: Virginia Counties") +
        theme_map() +
        scale_fill_viridis(
            option = "viridis",
            direction = -1,
            name = "Ratio",
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
                label.position = "bottom")) +
        theme(legend.position = "bottom",
              axis.text = element_text(size = 15),
              axis.title = element_text(size = 20),
              title = element_text(size = 20),
              plot.caption=element_text(hjust = 0.5))

    if(plot) {
        plot(usda.map)
    }else{
        return(usda.map)
    }
}

# Load the cleaned data files with our helper functions
# here, 3 indicates the health table

fea.data = read.clean.fea.data(data.dir, fea.tables, 6)
fea.map.data = merge.maps.and.data(fea.data)
# jpeg("output/USDA Maps/final_format/soda_price10.jpg", width = 1200, height = 800)
plot.usda(fea.map.data, "SODA_PRICE10", "VA")
# dev.off()
