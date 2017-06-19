# Synthetic Population: Change of Support
# prepare inputs for generating a synthetic population for Fairfax

# inputs:
#   Fairfax tax records (CoreLogic: Home Value, #Bedrooms, Zip Code, High School)
#   Fairfax PUMS (Housing: NP, Home Value, Bedrooms)
#   Fairfax PUMS (Housing + Person: NP, Home Value, Bedrooms, Race, Medicare, Medicaid)
#   Fairfax Zip Code demographic summaries (Total Population, Race, Medicare, Medicaid by Zip Code)
#   Fairfax High School area boundaries (fortify shapefile)
#   Fairfax Zip Code boundaries (fortify shapefile)

# output:
#   Fairfax High School demographic summaries (Total Population, Race, Medicare, Medicaid by High School)

# method:

# STEP 1: PREPARE INPUTS
#   attach zip code, high school area to CoreLogic
#   select variables needed from each dataset
#   attach home value, #bedrooms by house id to person PUMS
#   save the inputs as an RData file

# STEP 2: ESTIMATE HOUSING CHARACTERISTICS
#   useful predictors NP in both the housing PUMS and CoreLogic: home value, #bedrooms
#   impute #people using mice with these two predictors
#   replicate NP for each house, add missing (NA) demographics of interest
#   reweight to match zip code summaries for total population (from ACS)

# STEP 3: ESTIMATE DEMOGRAPHICS
#   impute race, medicare (y/n), medicaid (y/n) from person PUMS
#   reweight to match zip code demographic summaries

# Repeat steps 2+3 many times; each gives us one draw from our synthetic universe of Fairfax
# Then we can aggregate demographics by high school area; estimates and errors
# Finally, work on ease of use with generic functions

library(maptools)
library(dplyr)
library(tigris)

test <- read.csv("~/sdal/projects/hud_census/data/CoreLogic/2_WORKING/Fairfax_County_2013.csv")


# read in Fairfax CoreLogic data
ffx <- read.csv("~/sdal/projects/comm_fairfax/working/Fairfax_ALL_2013.csv")

# read VA housing PUMS
PUMS_housing <- read.csv("~/sdal/projects/comm_fairfax/working/synthetic_population/PUMS/ss14hva.csv") %>%
# read VA person PUMS
PUMS_person <- read.csv("~/sdal/projects/comm_fairfax/working/synthetic_population/PUMS/ss14pva.csv")

# filter to Fairfax county
va_pumas <- pumas("va")
ffx_pumas <- 59301:59309

PUMS_housing <- PUMS_housing %>% filter(PUMA %in% ffx_pumas)
PUMS_person <- PUMS_person %>% filter(PUMA %in% ffx_pumas)

# read in high school boundaries, zip code boundaries
zip <- readShapePoly("~/sdal/projects/limbo/fairfax_alerts/GISData/ZIP_Codes/ZIP_Codes.shp",
                     proj4string=CRS('+proj=longlat +ellps=WGS84'))
highSchool <- readShapePoly("~/sdal/projects/limbo/fairfax_alerts/GISData/High_School_Pyramids/High_School_Attendance_Areas.shp",
                            proj4string=CRS('+proj=longlat +ellps=WGS84'))


# attach zip code, high school area to CoreLogic data
ffx_coords <- ffx %>% select()
# problem: we don't have lat, long for Fairfax county?


# read ACS summaries by zip code
acs <- read.csv("~/sdal/projects/comm_fairfax/working/synthetic_population/ACSData_Fairfax_Zipcodesv2.csv")[,-1]



