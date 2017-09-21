library(maps)
library(ggplot2)
options(scipen = 100)
actSum = read.csv("./data/comm_fairfax/working/ATUS_data_working/ATUSsummaryAgeState.csv")



sleepPerState = with(actSum, tapply(t010101, state, mean))
sleepPerState = data.frame(state = tolower(names(sleepPerState)), avgsleep = sleepPerState)
rownames(sleepPerState) = NULL

stateMapData = merge(map_data("state"), sleepPerState, by.x = "region", by.y = "state")
dim(stateMapData)

midpt = mean(sleepPerState$avgsleep)/60
ggplot() + geom_polygon(data = stateMapData, aes(x=long, y = lat, group = group, fill = avgsleep/60)) +
  coord_fixed(1.3) +
  scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red', midpoint = midpt)


# Arguments are 1) a vector of activity times from the actSum data table, or a combination of such vectors. 2) a summary function which gets applied to activity.vector by each state. summary.name is a character string that labels the legend for the plot

atus.data.state = function(activity.vector, summary.function, summary.name = ""){

    byStateSum = tapply(activity.vector, actSum$state, summary.function)
    byStateSum = data.frame(state = tolower(names(byStateSum)), byStateSum)
    stateMapData = merge(map_data("state"), byStateSum, by.x = "region", by.y = "state")

    midpt = mean(stateMapData$byStateSum)
    plot1 = ggplot() + geom_polygon(data = stateMapData, aes(x=long, y = lat, group = group, fill = byStateSum)) +
        coord_fixed(1.3) + guides(fill=guide_legend(title = summary.name)) +
        scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red', midpoint = midpt)
    return(plot1)
}

# Get Craig's data and do some map based summaries

tokens = unique(substr(colnames(actSum)[28:451], 2, 3))
ncat = length(tokens)
category.list = vector("list", ncat)
names(category.list) = c("personal_care", "household_act", "helping_HH_mem", "helping_nHH_mem", "work_act", "education", "consum_purc", "prof_pers_care", "house_serv", "civic_obli", "eat_drink", "social_relax", "sports_rec", "religious", "volunteer", "telephone", "travel")

for(i in 1:ncat){
    # Find the columns with a particular token
    category.list[[i]] = actSum[,which(substr(colnames(actSum), 2, 3) == tokens[i])]
}

# Loop to make some maps.

map.out.dir = "./output/ATUS summary maps/"

for(i in 1:ncat){
    summary.name = names(category.list)[i]
    pdf(paste0(map.out.dir, summary.name, ".pdf"))
    plot(atus.data.state(rowSums(category.list[[i]]), mean, summary.name))
    dev.off()
}

# Do my aggregate exercise activities

exercise.cols = c("t030105", "t040105", "t050203", "t150301", grep("1301", colnames(actSum), value = T))
exercise.cols = exercise.cols[-c(9, 16, 18, 22, 38, 42)]

exercise.sum = rowSums(actSum[exercise.cols])

atus.data.state(exercise.sum, mean, "Total Exercise")
pdf(paste0(map.out.dir, "composite exercise", ".pdf"))
plot(atus.data.state(exercise.sum, mean, "Total Exercise"))
dev.off()


