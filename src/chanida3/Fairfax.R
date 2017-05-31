library(acs)
library(dplyr)
library(stringr)

#ARLINGTON CODE
#create a geo.set() using the geo.make() function
AC.VA<-geo.make(state="VA", county="Arlington", tract="*", block.group="*")
#Geo Set

#Getting the data - select the table at https://censusreporter.org/topics/table-codes/
#Total Population Table B01003
AC.VA.TP<-acs.fetch(geography=AC.VA, endyear=2015, table.number="B01003", col.name="pretty")
#Total Population Table B01001 by Sex x Age
AC.VA.TPAS<-acs.fetch(geography=AC.VA, endyear=2015, table.number="B01001",
                      table.name="Total Arlington Population Age x Sex (2011-2015 ACS)", col.name="pretty")

#Exporting the Data
ACStats<-data.frame(estimate(AC.VA.TP), round(standard.error(AC.VA.TP),0))
colnames(ACStats)<-c("Total","SE.Total")
write.csv(ACStats, file="./AC.Stats.csv")

#FAIRFAX
#THIS IS THE FAIRFAX CODE WITH THE B01003

FX.VA<-geo.make(zip.code = "*")
FX.VA.TP<-acs.fetch(geography=FX.VA, endyear=2015, table.number="B01003", col.name="pretty")

FXCStats<-data.frame(estimate(FX.VA.TP), round(standard.error(FX.VA.TP),0))
colnames(FXCStats)<-c("Total","SE.Total")

#Spliting the Stirng

#Establishing Zipcode for Fairfax
head(FXCStats)
FXCStats$zipcode <- row.names(FXCStats)
FXCStats$zipcode

#Spliting the String part and the numeric part of Zipcode
zip_split <- str_split(FXCStats$zipcode, pattern = " ", n = 2)
zipcode <- sapply(zip_split, function(x) x[2])
FXCStats$zipcode <- zipcode
#FXCStats

#Importing the Master ZipCode File
M_FX_Zip <- rio::import("~/git/comm_fairfax/data/fairfaxZipcodes.txt")
M_FX_Zip <- data.frame(M_FX_Zip)
colnames(M_FX_Zip) <- "zipcode"
M_FX_Zip$zipcode <- as.numeric(M_FX_Zip$zipcode)
FXCStats$zipcode <- as.numeric(FXCStats$zipcode)

#Joining the two tables by ZipCode
library(dplyr)
M_FX_Zip %>% left_join(FXCStats, by = "zipcode")


zip_matcher <- function(tablecode){
  #Get everything in zip code
  FX.VA<-geo.make(zip.code = "*")
  #Fetch the particular table
  FX.VA.TP<-acs.fetch(geography=FX.VA, endyear=2015, table.number=tablecode, col.name="pretty")
  #May need to modify depending on table
  FXCStats<-data.frame(estimate(FX.VA.TP), round(standard.error(FX.VA.TP),0))
  #I have deleted out the colnames. We can put them up when we need to.
  #colnames(FXCStats)<-c("Total","SE.Total")
  #Establishing Zipcode from Rownames
  FXCStats$zipcode <- row.names(FXCStats)
  FXCStats$zipcode
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

#Population by Age Group and Sex
pop_age_group <- zip_matcher("B01003")
write.csv(pop_age_group, file="~/git/comm_fairfax/data/comm_fairfax/working/Pop_Age_Group.csv")

#Population by Race/Ethnicity
pop_race_ethnicity <- zip_matcher("B03002")
head(pop_race_ethnicity)
write.csv(pop_race_ethnicity, file = "~/git/comm_fairfax/data/comm_fairfax/working/Pop_Race_Ethnicity.csv")

#Population insured by Medicaid
pop_insured_medicaid <- zip_matcher("B27010")
head(pop_insured_medicaid)
write.csv(pop_insured_medicaid, file = "~/git/comm_fairfax/data/comm_fairfax/working/pop_insured_medicaid.csv")

#Population by Poverty Status
pop_poverty_status <- zip_matcher("B17021")
head(pop_poverty_status)
write.csv(pop_poverty_status, file = "~/git/comm_fairfax/data/comm_fairfax/working/pop_poverty_status.csv")

#Household burden (Renters)
household_burden_renters <- zip_matcher("B25070")
head(household_burden_renters)
write.csv(household_burden_renters, file = "~/git/comm_fairfax/data/comm_fairfax/working/household_burden_renters.csv")

#Household burden (Owners)
household_burden_owners <- zip_matcher("B25091")
head(household_burden_owners)
write.csv(household_burden_owners, file = "~/git/comm_fairfax/data/comm_fairfax/working/household_burden_owners.csv")

#Households with no vehicle
household_no_vehicle <- zip_matcher("B25044")
head(household_no_vehicle)
write.csv(household_no_vehicle, file = "~/git/comm_fairfax/data/comm_fairfax/working/household_no_vehicle.csv")

#Familes w/ Children below Poverty
familes_children_below_poverty <- zip_matcher("B17010")
head(familes_children_below_poverty)
write.csv(familes_children_below_poverty, file = "~/git/comm_fairfax/data/comm_fairfax/working/familes_children_below_poverty.csv")

#Households below poverty
household_below_poverty <- zip_matcher("B17017")
head(household_below_poverty)
write.csv(household_below_poverty, file = "~/git/comm_fairfax/data/comm_fairfax/working/household_below_poverty.csv")

#Households receiving SNAP
household_receiving_snap <- zip_matcher("B22010")
head(household_receiving_snap)
write.csv(household_receiving_snap, file = "~/git/comm_fairfax/data/comm_fairfax/working/household_receiving_snap.csv")

#Populations by English proficiency
pop_eng_proficiency <- zip_matcher("B16004")
head(pop_eng_proficiency)
write.csv(pop_eng_proficiency, file = "~/git/comm_fairfax/data/comm_fairfax/working/pop_eng_proficiency.csv")



