# Synthetic Population: Change of Support
# prepare inputs for generating a synthetic population for Fairfax

# inputs:
#   Fairfax tax records (CoreLogic: Home Value, #Bedrooms, Zip Code, High School)
#

#   Fairfax PUMS (Housing: NP, Home Value, Bedrooms)
#   Fairfax PUMS (Housing + Person: NP, Home Value, Bedrooms, Race, Medicare, Medicaid) #Need this one
#   Attah


#   Fairfax Zip Code demographic summaries (Total Population, Race, Medicare, Medicaid by Zip Code)
#   Fairfax High School area boundaries (fortify shapefile)
#   Fairfax Zip Code boundaries (fortify shapefile)

# output:
#   Fairfax High School demographic summaries (Total Population, Race, Medicare, Medicaid by High School)

# method:

# STEP 1: PREPARE INPUTS
#   attach zip code, high school area to CoreLogic
#   select variables needed from each dataset
#   ???? Which each data set are u referring to?
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
library(rio)
library(mi)

test <- read.csv("~/sdal/projects/hud_census/data/CoreLogic/2_WORKING/Fairfax_County_2013.csv")


# read in Fairfax CoreLogic data
#Is this synthetic data? We are guessing it is.

ffx <- rio::import("~/git/comm_fairfax/data/comm_fairfax/working/Fairfax_ALL_2013.csv")
#Some explorations
length(names(ffx))
nrow(ffx)
unique(ffx$X.FIPS.CODE)
ffx$X.SITUS.HOUSE.NUMBER..2.

#Some error down here to be fixed later.
ffx<-apply(ffx, 2, function(x) gsub("^$|^ $", NA, x))
missing_df <- apply(ffx, 2, function (x) sum(is.na(x)))
View(missing_df)
missing_df <- t(missing_df)
missing_df <- data.frame(missing_df)
View(missing_df)
missing_df$X.SITUS.HOUSE.NUMBER..2.



# read VA housing PUMS
PUMS_housing <- rio::import("~/git/comm_fairfax/data/comm_fairfax/working/synthetic_population/PUMS/ss14hva.csv")
    # read VA person PUMS
length(names(PUMS_housing))
nrow(PUMS_housing)
PUMS_person <- read.csv("~/sdal/projects/comm_fairfax/working/synthetic_population/PUMS/ss14pva.csv")
length(names(PUMS_person))
match(names(PUMS_housing), names(PUMS_person))
names(PUMS_housing)
names(PUMS_person)

# filter to Fairfax county
va_pumas <- pumas("va") #download the puma shapefile for virginia
ffx_pumas <- 59301:59309     #shapefile code for ffx? guess

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



