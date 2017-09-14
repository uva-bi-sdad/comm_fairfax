actSum = read.csv("./git/comm_fairfax/data/comm_fairfax/working/ATUS_data_working/ATUSsummaryAgeState.csv")

####################
# Make some maps with ggplot2. You can ignore this for now. I need to flesh out this section
####################
sleepperstate = with(actSum, tapply(t010101, state, mean))
sleepperstate = data.frame(state = tolower(names(sleepperstate)), avgsleep = sleepperstate)
rownames(sleepperstate) = NULL

statemapdata = map_data("state")
statemapdata = merge(statemapdata, sleepperstate, by.x = "region", by.y = "state")
dim(statemapdata)

ggplot() + geom_polygon(data = statemapdata, aes(x=long, y = lat, group = group, fill = avgsleep/60)) + 
  coord_fixed(1.3)
