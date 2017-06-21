library(dplyr)
library(rio)

#Mental Health Providers
mh_provider <- rio::import("~/git/comm_fairfax/data/comm_fairfax/original/SAMSHA Behavioral_Health_Treament_Facility_listing_2017_06_09_150743.xlsx.xlsx")

View(mh_provider)
colnames(mh_provider)[1] <- "Name"
colnames(mh_provider)[2] <- "Type"
fairfaxzip <- rio::import("~/git/comm_fairfax/data/comm_fairfax/original/fairfaxZipcodes.txt")

nrow(fairfaxzip)
colnames(fairfaxzip)[1] <- "zip"

typeof(fairfaxzip$zip)
#Total Number of Fairfax Mental Health Providers
nrow(mh_provider)
mh_provider$zip <- as.numeric(mh_provider$zip)

sort(unique(mh_provider$county))

#Total Number of Fairfax Mental Health Providers
#Need to check zipcode file that Bianica gave has some errors

#Note there are some Arlington and Loudon County stuff in here.
mhp_fairfax <- mh_provider %>% inner_join(fairfaxzip, by = c("zip","zip"))
nrow(mhp_fairfax)

View(mhp_fairfax)

mhp_fairfax_only <- mh_provider %>%
                    filter(county == "Fairfax City" | county == "Fairfax")
nrow(mhp_fairfax_only)

View(mhp_fairfax_only)
#Data Observations
#Some organizations are listed more than once due to different locations
#As well as different types of services

#Make a relational data map

colnames(mhp_fairfax_only)
mhp_fairfax_clean <- mhp_fairfax_only[,c(1:3,7,10:11,33:35, 18:21)]
nrow(mhp_fairfax_clean)

#Mention Data Quality Issues
View(mhp_fairfax_clean)

mhp_zip_count <- mhp_fairfax_clean%>% group_by(zip) %>% summarize(count = n())
mhp_fairfax_clean <- mhp_fairfax_clean %>% left_join(mhp_zip_count, by = c("zip", "zip"))
write.csv(mhp_fairfax_clean, file = "/home/zarni90/git/comm_fairfax/data/comm_fairfax/original/mhp_clean.csv")

#mhp_fairfax_clean$count

