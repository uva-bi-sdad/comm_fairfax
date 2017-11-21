library(readxl)
library(dplyr)

bike_routes <- read_xls('data/comm_fairfax/working/rec_data/bike_routes_table.xls')
county_parks <- read_xls('data/comm_fairfax/working/rec_data/county_parks_table.xls')
county_trails <- read_xls('data/comm_fairfax/working/rec_data/county_trails_table.xls')
noncounty_parks <- read_xls('data/comm_fairfax/working/rec_data/noncounty_parks.xls')
noncounty_trails <- read_xls('data/comm_fairfax/working/rec_data/noncounty_trails_table.xls')
rec_impervious <- read_xls('data/comm_fairfax/working/rec_data/rec_impervious_table.xls')

join1 <- left_join(rec_impervious, county_parks, by = "GEOID")
join2 <- left_join(join1, county_trails,  by = "GEOID")
join3 <- left_join(join2, noncounty_parks, by = "GEOID")
join4 <- left_join(join3, noncounty_trails, by = "GEOID")
join5 <- left_join(join4, bike_routes, by = "GEOID")
cleaned <- join5[, -c(1,5,8,11,14,17)]

fairfax_bg <- read_xls('data/comm_fairfax/working/rec_data/ffx_bg_table.xls')
fairfax_bg <- fairfax_bg[, c(2,3,4,6,10)]
colnames(fairfax_bg)[5] <- "ALAND_sq_m"

rec_data <- left_join(fairfax_bg, cleaned, by = "GEOID")
#write.csv(rec_data, 'data/comm_fairfax/working/rec_data/rec_data_complete.csv')
