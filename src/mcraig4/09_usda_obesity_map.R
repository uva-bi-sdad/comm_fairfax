library(ggplot2)
library(maps)
library(dplyr)

county_obesity <- read.csv('data/comm_fairfax/working/mcraig4/health_usda.csv', stringsAsFactors = FALSE)
counties <- map_data("county")
data("state.fips")

# Minor changes to get county_obesity into usable form
county_obesity$County[1803] = "dona ana"
county_obesity$County = tolower(county_obesity$County)
# Minor changes to get state.fips into usable form
state.fips$polyname[20:22] = "massachusetts"
state.fips$polyname[23:24] = "michigan"
state.fips$polyname[34:37] = "new york"
state.fips$polyname[38:40] = "north carolina"
state.fips$polyname[53:55] = "virginia"
state.fips$polyname[56:60] = "washington"

# Join counties and state.fips
# Need the state abbreviation from the stat.fips frame
counties_st_ab <- left_join(counties, state.fips, by = c("region" = "polyname"))

# Merge on two variables so counties with the same name correspond to the right state
# This data is not usable for ggplot2
# Need to figure out how to get it into map_data()
obesity.map <- merge(counties_st_ab, county_obesity, by.x = c("subregion", "abb"), by.y = c("County", "State"))


MAP1<-(ggplot() +
           geom_polygon(data=obesity.map, aes(x=long, y=lat, group=group, fill=PCT_OBESE_ADULTS13), color="black", size=.25) +
           geom_polygon(data=counties, aes(x=long, y=lat, group=group), fill=NA, color="gray48", size=.25) +
           scale_fill_gradient(low = 'lightskyblue1', high = 'mediumblue'))

plot(MAP1)

# Make an additional VA Map
va_obesity <- subset(county_obesity, county_obesity$State == "VA")
