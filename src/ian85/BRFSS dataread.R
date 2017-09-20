library(foreign)
library(maps)
library(ggplot2)
options(stringsAsFactors = FALSE)
brfss <- read.xport("LLCP2016.xpt ")

hist(brfss$SLEPTIM1)

sleepPerState = with(brfss, tapply(SLEPTIM1, X_STATE, mean, na.rm = T))
sleepPerState = data.frame(state = tolower(names(sleepPerState)), avgsleep = sleepPerState)
mutate(sleepPerState, state = state.fips)
rownames(sleepPerState) = NULL


stateMapData = merge(map_data("state"), sleepPerState, by.x = "region", by.y = "state")
dim(stateMapData)

midpt = mean(sleepPerState$avgsleep)/60
ggplot() + geom_polygon(data = stateMapData, aes(x=long, y = lat, group = group, fill = avgsleep)) +
    coord_fixed(1.3) +
    scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red', midpoint = midpt)
