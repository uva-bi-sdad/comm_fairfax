
#title: "zip_matcher"
#author: "Zarni"
#date: "6/28/2017"
#output: html_document

##########################################################################################
#This set of functions take in a list of ACS table code and a list of zip codes and get all permutation of data set for each of the column. For now, the function to run with a list of table code is still under construction.
##########################################################################################

#Loading the needed data packages
library(acs)
library(dplyr)
library(stringr)

##########################################################################################
#Zip_Matcher: function takes in a single table code, a list of zipcodes (must be put in as character) and the year the ACS survey is taken
##########################################################################################

zip_matcher <- function(tablecode, ziplist, endyear){
    #Get data for all the zipcodes
    US<-geo.make(zip.code = "*")
    #Fetch the particular table
    US.TP<-acs.fetch(geography=US, endyear=endyear, table.number=tablecode, col.name="pretty")
    #Pulling out the Stats we want
    USStats<-data.frame(estimate(US.TP), round(standard.error(US.TP),0))
    #Establishing Zipcode from Rownames
    USStats$zipcode <- row.names(USStats)
    #Spliting the String part and the numeric part of Zipcode
    USStats$zipcode <- zip_acs_processing(USStats$zipcode)
    #Importing and Processing the Master ZipCode File
    #For leading zero zip codes, you should have it formatted as character to begin with.
    ziplist <- input_zip_processing(ziplist)
    ziplist$zipcode <- as.character(ziplist$zipcode)
    USStats$zipcode <- as.character(USStats$zipcode)
    #Joining the two tables by ZipCode
    US_Zip_Final <- ziplist %>% left_join(USStats, by = "zipcode")
    return (US_Zip_Final)
}

##########################################################################################
#zip_acs_processing: this helper function cleans the zipcodes from the ACS table. It takes in
#                    a list of zipcodes
##########################################################################################
zip_acs_processing <- function(zipcode){
    zip_split <- str_split(zipcode, pattern = " ", n = 2)
    zipcode <- sapply(zip_split, function(x) x[2])
    return(zipcode)
}

##########################################################################################
#input_zip_processing: this helper function cleans the zipcodes that the user provides and
#                      formats it as well as takes care of edge cases like leading "00"
##########################################################################################
input_zip_processing <- function(zipcode){
    zipcode <- as.data.frame(zipcode)
    colnames(zipcode) <- "zipcode"
    return (zipcode)
}

####################
###Some examples
####################
#Population by Age Group and Sex
#Getting the zipcodes of Fairfax county
ziplist <- rio::import("~/git/comm_fairfax/data/fairfaxZipcodes.txt")
pop_age_group_2015 <- zip_matcher("B01003", ziplist, 2015)
pop_age_group_2014 <- zip_matcher("B01003", ziplist, 2014)

#write.csv(pop_age_group, file="~/git/comm_fairfax/data/comm_fairfax/working/Pop_Age_Group.csv")

#Population by Race/Ethnicity
pop_race_ethnicity <- zip_matcher("B03002", ziplist)
View(pop_race_ethnicity)
#write.csv(pop_race_ethnicity, file = "~/git/comm_fairfax/data/comm_fairfax/working/Pop_Race_Ethnicity.csv")


FX.VA<-geo.make(zip.code = "*")
#Fetch the particular table
FX.VA.TP<-acs.fetch(geography=FX.VA, endyear=2015, table.number=, col.name="pretty")

