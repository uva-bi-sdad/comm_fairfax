library(maptools)
library(dplyr)
library(tigris)
library(rio)
library(mi)
library(mice)


#PUMS_PERSON data has a lot of NAs in it.
#Are we looking at complete sample of it?
#There is 1 year and 5 year PUMS
#Should I rename the variables? Should I refer to the technical document here
# https://www2.census.gov/programs-surveys/acs/tech_docs/pums/data_dict/PUMSDataDict15.pdf
#From here, create a list of variables we need to work on?

PUMS_person <- read.csv("~/sdal/projects/comm_fairfax/working/synthetic_population/PUMS/ss14pva.csv")

names(PUMS_person)
head(PUMS_person)
View(PUMS_person)
md.pattern(PUMS_person) #Missing data for sure
nrow(PUMS_person) # 83393


#Similar to Person data
PUMS_housing <- rio::import("~/git/comm_fairfax/data/comm_fairfax/working/synthetic_population/PUMS/ss14hva.csv")
names(PUMS_housing)

#Is this Geo file? What do we need? How do we use this?
va_pumas <- pumas("va") #download the puma shapefile for virginia


#ACS Summaries
acs_summaries <- read.csv("~/sdal/projects/comm_fairfax/working/synthetic_population/ACSData_Fairfax_Zipcodesv2.csv")[,-1]

head(acs_summaries)
names(acs_summaries)

#1487249
#Some of the
sum(acs_summaries$totalPopulation, na.rm = TRUE)
na_index <- which(is.na(acs_summaries$totalPopulation))

#Zip code without population numbers
zip_code_w_pop <- acs_summaries$zipcode[na_index]
length(zip_code_w_pop)
length(acs_summaries$zipcode)

