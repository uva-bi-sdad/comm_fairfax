####################################################################
#GOAL:
#This is to get the all the Zip Tabulated Areas for Fairfax County
#And also get the total precise population by 2010 Census
####################################################################

####################################################################
#REFERENCES: Zip Tabulated Area
#https://www.census.gov/geo/reference/zctas.html
#REFERENCES: Relationship between Zip Tabulated Area and County Codes
#https://www2.census.gov/geo/pdfs/maps-data/data/rel/explanation_zcta_county_rel_10.pdf
#REFERENCES: FIPS CODE EXPLANATION TABLE
#https://www.census.gov/geo/reference/codes/cou.html
#REFERENCES: County Codes FIPS codes for Virginia: Fairfax is: VA,51,059,Fairfax County,H1
#https://www2.census.gov/geo/docs/reference/codes/files/st51_va_cou.txt
#
#
#

zcta_all <- rio::import(file = "https://www2.census.gov/geo/docs/maps-data/data/rel/zcta_county_rel_10.txt")
typeof(zcta_all$GEOID)
fairfax <- zcta_all[zcta_all$GEOID ==51059,]
names(fairfax)
fairfax_pop <- fairfax[,c(1, 5)]
View(fairfax_pop)

write.csv(fairfax_pop, file = "~/git/comm_fairfax/data/comm_fairfax/working/fairfax_pop.csv")
write.csv(PUMS_person_interest, file = "~/git/comm_fairfax/data/comm_fairfax/working/PUMS_person_interest.csv")


#There are 49 ZCTA for Fairfax
nrow(fairfax)

#Total Population of Fairfax
sum(fairfax$POPPT)
#1081726 million

setwd("~/sdal/projects/limbo/fairfax_alerts/")
county <- readShapePoly("GISData/Fairfax_County_Border/Fairfax_County_Border.shp",
                        proj4string=CRS('+proj=longlat +ellps=WGS84'))
zip <- readShapePoly("GISData/ZIP_Codes/ZIP_Codes.shp",
                     proj4string=CRS('+proj=longlat +ellps=WGS84'))

shape_zip <- zip@data$ZIPCODE
match(shape_zip, fairfax$ZCTA5)


#Proof that we have the right zip codes
#https://factfinder.census.gov/faces/nav/jsf/pages/community_facts.xhtml?src=bkmk

#Zip Tabulated Areas
sort(fairfax$ZCTA5)


