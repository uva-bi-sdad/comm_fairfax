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
## Change to the name of your_csv in line 54 of the merge, and also change the by ##
## the the appropriate column names                                               ##
##                                                                                ##
## In ggplot at the bottom (lines 58 & 70), change fill to the variable you want  ##
## Ian is going to write a wrapper function for this                              ##
## Run the code, and it everything else should be automatic                       ##
####################################################################################

library(ggplot2)
library(maps)
library(dplyr)

your_csv <- read.csv('data/comm_fairfax/working/usda_data/health_usda.csv', stringsAsFactors = FALSE)
counties <- map_data("county")
data("state.fips")
states <- map_data('state')

# Minor changes to get county_obesity into usable form
your_csv$County[1803] = "dona ana"
your_csv$County = tolower(your_csv$County)
# Have to get rid of the . behind counties with the name like St. Mary's
your_csv$County = gsub('\\.','',your_csv$County)
your_csv$County = gsub("'",'', your_csv$County)
your_csv$County = gsub("\\s",'', your_csv$County)
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
obesity.map <- merge(counties_st_ab, your_csv, by.x = c("subregion", "abb"), by.y = c("County", "State"))
obesity.map <- obesity.map[order(obesity.map$order), ]

MAP1<-(ggplot() +
           geom_polygon(data=obesity.map, aes(x=long, y=lat, group=group, fill=PCT_OBESE_ADULTS13), color="black", size=.25) +
           geom_polygon(data=counties, aes(x=long, y=lat, group=group), fill=NA, color="gray48", size=.25) +
           geom_polygon(data=states, aes(x=long, y=lat, group=group), fill=NA, color="black", size=0.15) +
           scale_fill_gradient(low = "#56B4E9", high = "#E69F00", limits = range(obesity.map$PCT_OBESE_ADULTS13, na.rm = T), breaks = round(seq(min(obesity.map$PCT_OBESE_ADULTS13, na.rm = T), max(obesity.map$PCT_OBESE_ADULTS13, na.rm = T), length = 5), 1)) +
           coord_fixed(1.3) +
           labs(title = "Obesity Rates for 2013") +
           theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank(),
                 panel.background = element_blank(),
                 text = element_text(size = 20)))

plot(MAP1)

# Make an additional VA Map
va_obesity <- subset(obesity.map, obesity.map$abb == "VA")
VA_counties <- subset(counties, counties$region == "virginia")

VA_MAP<-(ggplot() +
             geom_polygon(data=va_obesity, aes(x=long, y=lat, group=group, fill=PCT_OBESE_ADULTS13), color="black", size=.25) +
             geom_polygon(data=VA_counties, aes(x=long, y=lat, group=group), fill=NA, color="gray48", size=.25) +
             scale_fill_gradient(low = "#56B4E9", high = "#E69F00", limits = range(obesity.map$PCT_OBESE_ADULTS13, na.rm = T), breaks = round(seq(min(obesity.map$PCT_OBESE_ADULTS13, na.rm = T), max(obesity.map$PCT_OBESE_ADULTS13, na.rm = T), length = 5), 1)) +
             coord_fixed(1.3) +
             labs(title = "Obesity Rates for 2013") +
             theme(axis.title=element_blank(),
                   axis.text=element_blank(),
                   axis.ticks=element_blank(),
                   panel.background = element_blank(),
                   text = element_text(size = 20)))

plot(VA_MAP)
