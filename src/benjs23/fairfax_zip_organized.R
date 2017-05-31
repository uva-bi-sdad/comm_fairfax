library(acs)
library(dplyr)
library(stringr)

######Commented out this block as redundant
######Delete in the future
#Importing the Master ZipCode File
#M_FX_Zip <- rio::import("~/git/comm_fairfax/data/fairfaxZipcodes.txt")
#M_FX_Zip <- data.frame(M_FX_Zip)
#colnames(M_FX_Zip) <- "zipcode"
#M_FX_Zip$zipcode <- as.numeric(M_FX_Zip$zipcode)
#FXCStats$zipcode <- as.numeric(FXCStats$zipcode)
#Joining the two tables by ZipCode
#M_FX_Zip %>% left_join(FXCStats, by = "zipcode")
#####
#####
FX.VA<-geo.make(zip.code = "*")
tablecode <- 'B01001'
#Fetch the particular table
FX.VA.TP<-acs.fetch(geography=FX.VA, endyear=2015, table.number=tablecode, col.name="pretty")

FXCStats<-data.frame(estimate(FX.VA.TP), round(standard.error(FX.VA.TP),0))



zip_matcher <- function(tablecode, year){

    FXCStats <-fetch_fairfax_datatable(tablecode, year)

    ########################################################################
    # I have deleted out the colnames. We can put them up when we need to. #
    # colnames(FXCStats)<-c("Total","SE.Total")                            #
    ########################################################################


    #Spliting the String part and the numeric part of Zipcode
    zip_split <- str_split(FXCStats$zipcode, pattern = " ", n = 2)
    zipcode <- sapply(zip_split, function(x) x[2])
    FXCStats$zipcode <- zipcode

    #Importing the Master ZipCode File
    M_FX_Zip <- rio::import("~/git/comm_fairfax/data/fairfaxZipcodes.txt")
    M_FX_Zip <- data.frame(M_FX_Zip)
    colnames(M_FX_Zip) <- "zipcode"
    M_FX_Zip$zipcode <- as.character(M_FX_Zip$zipcode)
    FXCStats$zipcode <- as.character(FXCStats$zipcode)

    #Joining the two tables by ZipCode
    FX_Zip_Final <- M_FX_Zip %>% left_join(FXCStats, by = "zipcode")
    return (FX_Zip_Final)
}

fetch_fairfax_datatable <- function(tablecode, year)
{
    #Create object to get all data by zip code
    FX.VA<-geo.make(zip.code = "*")

     #Fetch the US zip code table for a given table code and year
    FX.VA.TP<-acs.fetch(geography=FX.VA, endyear= year, table.number= tablecode, col.name"pretty")

    #Create data table
    #May need to modify depending on table
    FXCStats<-data.frame(estimate(FX.VA.TP), round(standard.error(FX.VA.TP),0))

    #Labelling rows with header 'zipcode
    FXCStats$zipcode <- row.names(FXCStats)

    return(FXCStats)
}



#Population by Age Group and Sex
pop_age_group <- zip_matcher("B01003")
write.csv(pop_age_group, file="~/git/comm_fairfax/data/comm_fairfax/working/Pop_Age_Group.csv")

#Population by Race/Ethnicity
pop_race_ethnicity <- zip_matcher("B03002")
head(pop_race_ethnicity)
write.csv(pop_race_ethnicity, file = "~/git/comm_fairfax/data/comm_fairfax/working/Pop_Race_Ethnicity.csv")
