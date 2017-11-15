library(foreign)
library(maps)
library(ggplot2)
options(stringsAsFactors = FALSE)

years = 2003:2016
lead0 = c(paste0("0", 3:9), 10)
file.part1 = paste0(2003:2010, "/files/", "CDBRFS", lead0, "XPT.zip")
file.part2 = paste0(2011:2016, "/files/", "LLCP", 2011:2016, "XPT.zip")
file.part = c(file.part1, file.part2)
file.strings = paste0("https://www.cdc.gov/brfss/annual_data/", file.part)

for(i in 1:length(years)) download.file(file.strings[i], paste0("./data/comm_fairfax/original/BRFSS/brffs", years[i], ".zip"))

for(i in 1:length(years)) unzip(paste0("./data/comm_fairfax/original/BRFSS/brffs", years[i], ".zip"), exdir = "./data/comm_fairfax/original/BRFSS/")

data.files = list.files("./data/comm_fairfax/original/BRFSS/", pattern = ".[Xx][Pp][Tt]")

aggregations = vector("list", length(years))

for(i in 1:length(years)){
    # Load in active file
    print(i)
    active.file = data.files[i]
    brfss <- read.xport(paste0("./data/comm_fairfax/original/BRFSS/", active.file))

    # Aggregate BMI by state
    column = grep("bmi", colnames(brfss), ignore.case = T)[1]
    rows = brfss[, column] < 9999

    bmi = tapply(brfss[rows,column], brfss$X_STATE[rows], mean)/100
    aggregations[[i]] = data.frame(state = as.numeric(names(bmi)), year = years[i], bmi = bmi)
    rm(brfss)
}

bmiStateYear = data.frame(state = numeric(0), year = numeric(0), bmi = numeric(0))

for(i in 1:length(years)) bmiStateYear = rbind(bmiStateYear, aggregations[[i]])

write.csv(bmiStateYear, "./data/comm_fairfax/working/bmiStateYearBRFSS.csv", row.names = F)
