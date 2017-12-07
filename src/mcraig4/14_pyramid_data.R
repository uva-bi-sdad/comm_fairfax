library(readxl)
library(dplyr)

# Load all the individual recreational usage files
bike_routes <- read_xls('data/comm_fairfax/working/pyramid_data/bike_routes_pyramid.xls')
county_parks <- read_xls('data/comm_fairfax/working/pyramid_data/county_parks_pyramid.xls')
county_trails <- read_xls('data/comm_fairfax/working/pyramid_data/county_trails_pyramid.xls')
noncounty_parks <- read_xls('data/comm_fairfax/working/pyramid_data/noncounty_parks_pyramid.xls')
noncounty_trails <- read_xls('data/comm_fairfax/working/pyramid_data/noncounty_trails_pyramid.xls')
rec_impervious <- read_xls('data/comm_fairfax/working/pyramid_data/rec_impervious_pyramid.xls')

# Join them together to get one table
join1 <- left_join(rec_impervious, county_parks, by = "SCHOOL_NAM")
join2 <- left_join(join1, county_trails,  by = "SCHOOL_NAM")
join3 <- left_join(join2, noncounty_parks, by = "SCHOOL_NAM")
join4 <- left_join(join3, noncounty_trails, by = "SCHOOL_NAM")
join5 <- left_join(join4, bike_routes, by = "SCHOOL_NAM")
# Clean to get rid of all the OBJECTIDs
pyramid_data <- join5[, -c(1,5,8,11,14,17)]

# Clean dataframe to change NAs to 0
pyramid_data[is.na(pyramid_data)] <- 0
write.csv(pyramid_data, 'data/comm_fairfax/working/pyramid_data/pyramid_data_complete.csv')

