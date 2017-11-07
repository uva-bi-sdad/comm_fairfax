library(ggplot2)
library(maps)
library(dplyr)
library(stringr)

county_obesity <- read.csv('data/comm_fairfax/working/mcraig4/health_usda.csv', stringsAsFactors = FALSE)
counties <- map_data("county")
data("state.fips")

# Minor changes to get county_obesity into usable form
county_obesity$County[1803] = "dona ana"
county_obesity$County = tolower(county_obesity$County)

# Join counties and state.fips, so you can get the FIPS code for each state in this dataframe
counties_st_ab <- left_join(counties, state.fips, by = c("region" = "polyname"))
# NEED TO CHANGE POLYNAME LEVELS
# NEW YORK IS newyork:main

obesity.map <- merge(counties_st_ab, county_obesity, by.x = c("subregion", "abb"), by.y = c("County", "State"))

MAP1<-(ggplot() +
           geom_polygon(data=obesity.map, aes(x=long, y=lat, group=group, fill=PCT_OBESE_ADULTS13), color="black", size=.25) +
           geom_polygon(data=counties, aes(x=long, y=lat, group=group), fill=NA, color="gray48", size=.25) +
           scale_fill_gradient(low = 'lightskyblue1', high = 'mediumblue'))

plot(MAP1)

# Make an additional VA Map
va_obesity <- subset(county_obesity, county_obesity$State == "VA")
