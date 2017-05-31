library(acs)


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

#FairFax

FX.VA<-geo.make(zip.code = "*")
FX.VA.TP<-acs.fetch(geography=FX.VA, endyear=2015, table.number="B01003", col.name="pretty")

FXCStats<-data.frame(estimate(FX.VA.TP), round(standard.error(FX.VA.TP),0))
colnames(FXCStats)<-c("Total","SE.Total")

#Spliting the Stirng
library("stringr")

head(FXCStats)
FXCStats$zipcode <- row.names(FXCStats)
FXCStats$zipcode

zip_split <- str_split(FXCStats$zipcode, pattern = " ", n = 2)
zipcode <- sapply(zip_split, function(x) x[2])
FXCStats$zipcode <- zipcode
FXCStats

M_FX_Zip <- rio::import("~/git/community_indicators/data/community_indicators/original/fairfaxZipcodes.txt")
M_FX_Zip <- data.frame(M_FX_Zip)
colnames(M_FX_Zip) <- "zipcode"
M_FX_Zip$zipcode <- as.numeric(M_FX_Zip$zipcode)
FXCStats$zipcode <- as.numeric(FXCStats$zipcode)



library(dplyr)
M_FX_Zip %>% inner_join(FXCStats, by = "zipcode")




