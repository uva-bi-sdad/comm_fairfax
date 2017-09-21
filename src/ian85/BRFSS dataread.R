library(foreign)
library(maps)
library(ggplot2)
options(stringsAsFactors = FALSE)
brfss <- read.xport("./data/comm_fairfax/original/LLCP2016.xpt")
statefips = read.csv("./data/comm_fairfax/original/ATUS_Data/statefips.csv", header = T, stringsAsFactors = F)


sleepPerState = with(brfss, tapply(SLEPTIM1, X_STATE, mean, na.rm = T))[-c(52:54)]

sleepPerState = data.frame(state = tolower(statefips$state[match(statefips$statefips, as.numeric(names(sleepPerState))) ]), avgsleep = sleepPerState)
mutate(sleepPerState, state = state.fips)
rownames(sleepPerState) = NULL


stateMapData = merge(map_data("state"), sleepPerState, by.x = "region", by.y = "state")
dim(stateMapData)

midpt = mean(sleepPerState$avgsleep)
ggplot() + geom_polygon(data = stateMapData, aes(x=long, y = lat, group = group, fill = avgsleep)) +
    coord_fixed(1.3) +
    scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red', midpoint = midpt)
