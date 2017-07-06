library(maptools) #For mapping
library(dplyr) #For linking tables
library(tigris) #For mapping
library(rio) #For loading
library(mi) # For imputation
library(mice) #The imputation package

#Loading the Personal PUMS for 2015
PUMS_person <- rio::import("~/git/comm_fairfax/data/comm_fairfax/working/ss15pva.csv")
to_match_var <- names(PUMS_person)
interest_variables <- c("PAP", "FINCP", "HINCP", "NOC", "NRC", "PAOC", "HINS3", "HINS4", "HINS1", "HINS5", "HINS6", "HINS7", "VEH", "RNTP")

names(PUMS_person)





match(to_match_var, interest_variables)
to_match_var <- data.frame(to_match_var)
interest_variables <- data.frame(interest_variables)
colnames(to_match_var) <- "varname"
colnames(interest_variables) <- "varname"

to_match_var %>% inner_join(interest_variables, by = ("varname" = "varname"))

#Does it include the variables that I am interested in right now?
#FS




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
#1487249
na_index <- which(is.na(acs_summaries$totalPopulation))

#Zip code without population numbers
zip_code_w_pop <- acs_summaries$zipcode[na_index]
length(zip_code_w_pop)
length(acs_summaries$zipcode)

#VA,51,059,Fairfax County,H1

