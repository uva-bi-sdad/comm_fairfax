
####################
# Only run this if something happened to the merged file I put in the working directory of the data file. I may also come back later and use this script to keep all my merging code.
####################

# Load in data
CPS = read.csv("./data/comm_fairfax/original/ATUS_Data/atuscps_0316.dat")
timeSum = read.csv("./data/comm_fairfax/original/ATUS_Data/atussum_0316.dat", header = T, stringsAsFactors = F)
statefips = read.csv("./data/comm_fairfax/original/ATUS_Data/statefips.csv", header = T, stringsAsFactors = F)

#Add age group to timeSum
timeSum$agegrp = ifelse(timeSum$TEAGE <= 19, 1, ifelse(timeSum$TEAGE <= 64, 2, 3))

# timeSum has unique TUCASEID's, age, and a breakdown of how time was spent
# Make a lookup table for each case id's state
casestate = CPS[,c(1, 8)]
casestate = casestate[match(unique(casestate$TUCASEID), casestate$TUCASEID),]
# Remove hawaii and alaska (GESTFIPS = 15 and 2)
casestate = subset(casestate, !(GESTFIPS %in% c(2, 15)))

# get the state names in there
casestate = merge(casestate, statefips[,-3], by.x = "GESTFIPS", by.y = "statefips", all.x = T)
# Merge state data frame with summary data frame
casestatesum = merge(casestate, timeSum)

write.csv(casestatesum, "./git/comm_fairfax/data/comm_fairfax/working/ATUS_data_working/ATUSsummaryAgeState.csv")

