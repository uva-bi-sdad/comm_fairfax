library(readxl)
library(dplyr)

# Load all the individual recreational usage files
bike_routes <- read_xls('data/comm_fairfax/working/rec_data/bike_routes_table.xls')
county_parks <- read_xls('data/comm_fairfax/working/rec_data/county_parks_table.xls')
county_trails <- read_xls('data/comm_fairfax/working/rec_data/county_trails_table.xls')
noncounty_parks <- read_xls('data/comm_fairfax/working/rec_data/noncounty_parks.xls')
noncounty_trails <- read_xls('data/comm_fairfax/working/rec_data/noncounty_trails_table.xls')
rec_impervious <- read_xls('data/comm_fairfax/working/rec_data/rec_impervious_table.xls')
colnames(rec_impervious)[3] <- "recimpervious_AREA"

# Join them together to get one table
join1 <- left_join(rec_impervious, county_parks, by = "GEOID")
join2 <- left_join(join1, county_trails,  by = "GEOID")
join3 <- left_join(join2, noncounty_parks, by = "GEOID")
join4 <- left_join(join3, noncounty_trails, by = "GEOID")
join5 <- left_join(join4, bike_routes, by = "GEOID")
# Clean to get rid of all the OBJECTIDs
cleaned <- join5[, -c(1,5,8,11,14,17)]

# Load the table of Fairfax County and Fairfax City block groups (666 BGs)
fairfax_bg <- read_xls('data/comm_fairfax/working/rec_data/ffx_bg_table.xls')
# Clean to get rid of columns that are not important
fairfax_bg <- fairfax_bg[, c(2,3,4,6,10)]
# ALAND's unit is in sq meters
colnames(fairfax_bg)[5] <- "ALAND_sq_m"

# Join rec_data with the Fairfax BGs
rec_data <- left_join(fairfax_bg, cleaned, by = "GEOID")
# Clean dataframe to change NAs to 0
rec_data[is.na(rec_data)] <- 0
#write.csv(rec_data, 'data/comm_fairfax/working/rec_data/rec_data_complete.csv')
