library(ggplot2)
library(maps)

county_obesity <- read.csv('data/comm_fairfax/working/mcraig4/health_usda.csv', stringsAsFactors = FALSE)
counties <- map_data("county")

obesity.map <- left_join(counties, county_obesity, by = c("subregion" = "County"))

MAP1<-(ggplot() +
           geom_polygon(data=obesity.map, aes(x=long, y=lat, group=group, fill=PCT_OBESE_ADULTS13), color="black", size=.25) +
           geom_polygon(data=counties, aes(x=long, y=lat, group=group), fill=NA, color="gray48", size=.25))

plot(MAP1)
plot(us_counties)
