#install.packages(c("ggplot2", "devtools", "dplyr", "stringr", "maps", "maptools"))
library(maps)
library(ggmap)
library(maptools)
library(ggplot2)

# Function to turn the state/county numbers into a string for the map function
fips2string = function(fips){
  sfip = fips[1]
  cfip = fips[2]
  state = statefips[statefips$statefips == sfip, 1]
  county = subset(countyfips, statefips == sfip & countyfips == cfip)$county
  county = strsplit(county, " ")[[1]][1]
  return(paste(state, county, sep = ','))
}



# Load in data

options(scipen = 20)
timeUse = read.csv("./Data/atusact_0316.dat", header = T, stringsAsFactors = F)
CPS = read.csv("./Data/atuscps_0316.dat", header = T, stringsAsFactors = F)
timeSum = read.csv("./Data/atussum_0316.dat", header = T, stringsAsFactors = F)
statefips = read.csv("./Data/statefips.csv", header = T, stringsAsFactors = F)
countyfips = read.csv("./Data/countyfips.csv", header = T, stringsAsFactors = F)

#Add age group to timeSum
timeSum$agegrp = ifelse(timeSum$TEAGE <= 19, 1, ifelse(timeSum$TEAGE <= 64, 2, 3))

#######
# CPS has location. state: GESTFIPS, county: GTCO
# County data is pretty sparse. This is the code I wrote to work with that, but I don't think it's gonna pan out for now.
# 
# caseidLocation = CPS[,c(1, 8, 10)]
# locsubset = subset(caseidLocation, substr(TUCASEID, 1, 4) == "2016")
# # Remove alaska and hawaii (FIPS = 2 and 15), also entries whose counties are unidentified
# locsubset = subset(locsubset, !(GESTFIPS %in% c(2, 15)) & GTCO != 0)
# # Extract unique TUCASEIDs
# locsubset = locsubset[match(unique(locsubset$TUCASEID), locsubset$TUCASEID),]
#####


# timeSum has unique TUCASEID's, age, and a breakdown of how time was spent
# Make a lookup table for each case id's state
casestate = CPS[,c(1, 8)]
casestate = casestate[match(unique(casestate$TUCASEID), casestate$TUCASEID),]
# Remove hawaii and alaska
casestate = subset(casestate, !(GESTFIPS %in% c(2, 15)))

# get the state names in there
casestate = merge(casestate, statefips[,-3], by.x = "GESTFIPS", by.y = "statefips", all.x = T)
# Merge state data frame with summary data frame
casestatesum = merge(casestate, timeSum)

# Now we can start making summaries of time spent doing various activities and visualize them
# Sleep is t010101. Let's see how much people sleep on average per state

sleepperstate = with(casestatesum, tapply(t010101, state, mean))
sleepperstate = data.frame(state = tolower(names(sleepperstate)), avgsleep = sleepperstate)
rownames(sleepperstate) = NULL

# Make some maps with ggplot2

statemapdata = map_data("state")
statemapdata = merge(statemapdata, sleepperstate, by.x = "region", by.y = "state")
dim(statemapdata)

ggplot() + geom_polygon(data = statemapdata, aes(x=long, y = lat, group = group, fill = avgsleep/60)) + 
  coord_fixed(1.3)










