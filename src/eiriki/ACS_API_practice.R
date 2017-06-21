library(acs)
#To download data via the ACS API you need to request "key" from Census
#http://api.census.gov/data/key_signup.html

#install the key (you only need to do this once)
api.key.install(key="0c0dbdc056ebec0f9637cb5fbdba46fa03ca8afc")

#create a geo.set() using the geo.make() function
FX.VA<-geo.make(zip.code = "*")




#Getting the data - select the table at https://censusreporter.org/topics/table-codes/
#Total Population Table B01003
FX.VA.TP<-acs.fetch(geography=FX.VA, endyear=2015, table.number="B01001", col.name="pretty")
#Total Population Table B01001 by Sex x Age
FX.VA.TPAS<-acs.fetch(geography=FX.VA, endyear=2015, table.number="B01001",
                      table.name="Total Arlington Population Age x Sex (2011-2015 ACS)", col.name="pretty")

#Exporting the Data
FXStats<-data.frame(estimate(FX.VA.TP), round(standard.error(FX.VA.TP),0))
colnames(FXStats)<-c("Total","SE.Total")

#splitting the data to get zip codes
library(stringr)
library(dplyr)

#changing zipcode to a column
head(FXStats)
FXStats$zipcode <- row.names(FXStats)
FXStats$zipcode

#physically splitting the code with str_split
zip_split <- str_split(FXStats$zipcode, pattern = " ", n= 2)
zipcode <- sapply(zip_split, function(x) x[2])
FXStats$zipcode = zipcode

#read in zip code text file
ffx_zip_txt <- rio::import("~/git/lab/comm_fairfax/data/fairfaxZipcodes.txt")
ffx_zip_df = data.frame(ffx_zip_txt)
colnames(ffx_zip_df) <- "zipcode"

ffx_zip_df$zipcode <- as.numeric(ffx_zip_df$zipcode)
FXStats$zipcode <- as.numeric(FXStats$zipcode)

ffx_zip_df$zipcodes %>% inner_join(FXStats, by = "zipcode")
