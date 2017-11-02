library(rgdal)
library(ggplot2)
library(dplyr)

county_obesity <- read.csv('data/comm_fairfax/working/mcraig4/health_usda.csv')
us_counties <- readOGR(dsn = 'data/comm_fairfax/working/mcraig4/us_counties', layer = "tl_2016_us_county")


#To make it plottable: Bianica's code
us_counties@data$id<-rownames(us_counties@data) #Information on each block group
us_counties.points<-fortify(us_counties, region="id") #Lat and Long of the block groups
us_counties.df<-left_join(us_counties.points, us_counties@data, by="id") #Join it to the data set with Lat and Log and GEOID
us_counties.df$GEOID<-as.numeric(paste(us_counties.df$GEOID)) # This is converting the census block group into GEOID

obesity.map <- left_join(county_obesity, us_counties.df, by = c("FIPS" = "GEOID"))

MAP1<-(ggplot() +
           geom_polygon(data=us_counties.df, aes(x=long, y=lat, group=group), fill=NA, color="gray48", size=.25) +
           geom_polygon(data=obesity.map, aes(x=long, y=lat, group=group, fill=PCT_OBESE_ADULTS13), color="black", size=.25))

plot(MAP1)
plot(us_counties)
