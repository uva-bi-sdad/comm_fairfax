library(maptools) #For mapping
library(dplyr) #For linking tables
library(tigris) #For mapping
library(rio) #For loading
library(mi) # For imputation
library(mice) #The imputation package

#Loading the Personal PUMS for 2015
PUMS_person <- rio::import("~/git/comm_fairfax/data/comm_fairfax/final/pums_ffx.csv")
names(PUMS_person)
to_match_var <- names(PUMS_person)
PUMS_person_interest <- c("PINCP", "DREM", "ENG", "PAP", "RAC1P", "SEX", "AGEP")
match(PUMS_person_interest,to_match_var)

head(PUMS_person[PUMS_person_interest,])

nrow(!is.na(PUMS_person))
head(PUMS_person)
names(PUMS_person)

#Match the columns that we want to look at
index_interest_variable <- (match(PUMS_person_interest,names(PUMS_person)))
index_interest_variable <- index_interest_variable[!is.na(index_interest_variable)]
index_interest_variable

PUMS_person_interest <- PUMS_person[,index_interest_variable]
ncol(PUMS_person_interest)
md.pattern(PUMS_person_interest)

#Writing the PUMS_person_interest data file we are hoping to impute by.
write.csv(PUMS_person_interest, file = "~/git/comm_fairfax/data/comm_fairfax/working/PUMS_person_interest.csv")
