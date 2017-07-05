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

#There are 49 ZCTA for Fairfax
nrow(fairfax)

#Total Population of Fairfax
sum(fairfax$POPPT)
#1081726 million

#Proof that we have the right zip codes
#https://factfinder.census.gov/faces/nav/jsf/pages/community_facts.xhtml?src=bkmk

#Zip Tabulated Areas
sort(fairfax$ZCTA5)


