####################################################################################
## This code allows you to produce a map on the county levels for each table      ##
## looked at from the USDA DataDownload                                           ##
##                                                                                ##
## To produce the maps for the table you are looking at, change line 24 to the    ##
## path of csv you want to use                                                    ##
##                                                                                ##
## Assign it a name                                                               ##
##                                                                                ##
## Change lines 30-35 to get your dataframe into usable format                    ##
##                                                                                ##
## Change to the name of the csv in line 54 & 55 of the merge, and also change    ##
##                                                                                ##
##                                                                                ##
## Run Ian's wrapper function. Change column in lines 85/85                       ##
####################################################################################

library(ggplot2)
library(maps)
library(dplyr)

FEAstores <- read.csv('data/comm_fairfax/working/usda_data/FEAstores.csv', stringsAsFactors = FALSE)
counties <- map_data("county")
data("state.fips")
states <- map_data('state')

# Minor changes to get county_obesity into usable form
FEAstores$County[1803] = "dona ana"
FEAstores$County = tolower(FEAstores$County)
# Have to get rid of the . behind counties with the name like St. Mary's
FEAstores$County = gsub('\\.','',FEAstores$County)
FEAstores$County = gsub("'",'', FEAstores$County)
FEAstores$County = gsub("\\s",'', FEAstores$County)
# Minor changes to get state.fips into usable form
state.fips$polyname[20:22] = "massachusetts"
state.fips$polyname[23:24] = "michigan"
state.fips$polyname[34:37] = "new york"
state.fips$polyname[38:40] = "north carolina"
state.fips$polyname[53:55] = "virginia"
state.fips$polyname[56:60] = "washington"
# Minor changes to get counties in working form
counties$subregion = gsub('\\.','',counties$subregion)
counties$subregion = gsub("'",'', counties$subregion)
counties$subregion = gsub("\\s",'', counties$subregion)

# Join counties and state.fips
# Need the state abbreviation from the stat.fips frame
counties_st_ab <- left_join(counties, state.fips, by = c("region" = "polyname"))

# Merge on two variables so counties with the same name correspond to the right state
# Got line 54 from looking through ?map_data
stores.map <- merge(counties_st_ab, FEAstores, by.x = c("subregion", "abb"), by.y = c("County", "State"))
stores.map <- stores.map[order(stores.map$order), ]

# Here I'm combining the functionality of the previous two map functions into a single wrapper function. This will cut down on the number of things needed to change to plot new variables and new regions.

# here, colname is a string giving the name of the column with which to fill. state is a string giving the state abberviation, such as 'VA' or 'CA'. If plot = T it will plot the map, if plot = F it saves the ggplot() object and returns it

plot.usda = function(data, colname, state, plot = T, title){

    limits = range(data[,colname], na.rm = T)
    breaks = round(seq(min(data[,colname], na.rm = T), max(data[,colname], na.rm = T), length = 5), 1)
    if(missing(title)) title = colname
    if(!missing(state)) data = subset(data, stores.map$abb == state)

    usda.map = ggplot() +
               geom_polygon(data=data, aes(x=long, y=lat, group=group, fill=data[,colname]), color="black", size=.25) +
               geom_polygon(data=data, aes(x=long, y=lat, group=group), fill=NA, color="gray48", size=.25) +
               #geom_polygon(data=states, aes(x=long, y=lat, group=group), fill=NA, color="black", size=0.15) +
               scale_fill_gradient(low = "#E69F00", high = "#56B4E9", limits = limits, breaks = breaks) +
               coord_fixed(1.3) +
        labs(title = title, fill = colname) +
        theme(axis.title=element_blank(),
              axis.text=element_blank(),
              axis.ticks=element_blank(),
              text = element_text(size = 20))

    if(plot) {
        plot(usda.map)
    }else{
        return(usda.map)
    }
}

plot <- plot.usda(stores.map, "GROCPTH14")
plot_va <-plot.usda(stores.map, "GROCPTH14", "VA")

va.map = plot.usda(stores.map, "PCT_OBESE_ADULTS13", "VA", plot = F)
