library(maps)
library(ggplot2)
library(dplyr)
library(reshape2)
library(car)
options(scipen = 100)
actSum = read.csv("./data/comm_fairfax/working/ATUS_data_working/ATUSsummaryAgeState.csv")
map.out.dir = "./output/ATUS summary maps/"


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


# Clean up some chloropleths with some tlc, do line graphs for to show trends
# exercise & bmi: 2003, 2016


bmiStateYear = read.csv("./data/comm_fairfax/working/ATUS_data_working/ATUS state year files/Avg_BMI.csv")
colnames(bmiStateYear)[3] = "bmi"
exercise.cols = c("t030105", "t040105", "t050203", "t150301", grep("1301", colnames(actSum), value = T))
exercise.cols = exercise.cols[-c(9, 16, 18, 22, 38, 42)]

for(year in c(2003, 2016)){
    rows = which(actSum$TUYEAR %in% c(2003, 2016))
    exercise.sum = cbind(year = actSum$TUYEAR[rows], minutes = rowSums(actSum[rows, exercise.cols]))


    byStateSum = melt(tapply(exercise.sum[,2], list(actSum$TUYEAR[rows], actSum$state[rows]), mean))
    byStateSum[,2] = tolower(byStateSum[,2])
    colnames(byStateSum) = c("Year", "state", "Minutes")
    stateMapData = merge(map_data("state"), byStateSum, by.x = "region", by.y = "state")
    stateMapData = arrange(stateMapData, stateMapData$order)

    midpt = 12
    plot1 = ggplot() + geom_polygon(data = stateMapData[stateMapData$Year == year,], aes(x=long, y = lat, group = group, fill = Minutes)) +
        coord_fixed(1.3) + guides(fill=guide_legend(title = "Exercise Summary")) +
        scale_fill_gradient(low = 'red', high = 'blue',
                            breaks = round(quantile(stateMapData$Minutes, seq(0, 1, .2), names = F), 1),
                            limits = range(stateMapData$Minutes)) +
        labs(title = paste("Exercise in minutes per day for", year)) +
        theme(axis.title=element_blank(),
              axis.text=element_blank(),
              axis.ticks=element_blank(),
              text = element_text(size = 20)) +
        geom_path(data = map_data("state") ,aes( x = long , y = lat , group=group ), colour = "black")

    pdf(paste0(map.out.dir, "exercise ", year, ".pdf"))
    plot(plot1)
    dev.off()

    bmisummary = bmiStateYear
    bmimapdata = merge(map_data("state"), bmisummary, by.x = "region", by.y = "state")
    bmimapdata = arrange(bmimapdata, bmimapdata$order)

    plot2 = ggplot() + geom_polygon(data = bmimapdata[bmimapdata$year == year,], aes(x=long, y = lat, group = group, fill = bmi)) +
        coord_fixed(1.3) + guides(fill=guide_legend(title = "Body Mass Index")) +
        scale_fill_gradient(low = 'blue', high = 'red',
                             breaks = round(quantile(bmimapdata$bmi, seq(0, 1, .2), names = F), 1),
                            limits = range(bmimapdata$bmi)) +
        labs(title = paste("Average BMI for", year)) +
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          text = element_text(size = 20)) +
    geom_path(data = map_data("state") ,aes( x = long , y = lat , group=group ), colour = "black")

    pdf(paste0(map.out.dir, "bmi ", year, ".pdf"))
    plot(plot2)
    dev.off()
}





# Line graphs
# Age groups: 1 = "15-19", 2 = "20-64", 3 = "65+"
dataYearByAge = read.csv("./data/comm_fairfax/working/ATUS trend summary data.csv", row.names = 1)
head(dataYearByAge)
head(bmiStateYear)

active.data = aggregate(bmi ~ year, bmiStateYear[,-1], mean)

bmiplot = ggplot(data = active.data, aes(x = year, y = bmi)) +
    geom_line() + geom_point(size = 4) +
    labs(x = "Year", y = "BMI", title = "BMI by Year") +
    theme(text = element_text(size = 20))

pdf(paste0(map.out.dir, "bmi.pdf"))
plot(bmiplot)
dev.off()

# Line graph of energy availability

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

energy = read.csv("./data/comm_fairfax/original/food energy.csv")
obesity = read.csv("./data/comm_fairfax/original/prevalence-of-obesity-in-adults-by-region.csv")

en.ob = merge(energy, subset(obesity, obesity$Entity =="North America and Europe" ))
colnames(en.ob)[c(2, 5)] = c("Energy", "Obesity")
en.ob = subset(en.ob, select = c(Year, Energy, Obesity))
en.ob[,-1] = scale(en.ob[,-1])
en.ob = melt(en.ob, measure.vars = c("Energy", "Obesity"))
energyplot = ggplot(data = en.ob, aes(x = Year, y = value, color = variable)) +
    geom_line() + geom_point(size = 4) +
    labs(y = "Scaled Energy Availability and Obesity Rates", title = "Available Food Energy and Obesity", color = "") +
    scale_color_manual(values=c("#E69F00", "#56B4E9")) +
    theme(text = element_text(size = 20))


pdf(paste0(map.out.dir, "energy and obesity.pdf"))
plot(energyplot)
dev.off()

# Line graph for exercise
head(dataYearByAge)

active.data = subset(dataYearByAge, ATUScat == "exercise_summary")

exerciseplot = ggplot(data = active.data, aes(x = year, y = value, color = as.factor(agegrp))) +
    geom_line() + geom_point(size = 4) +
    labs(y = "Minutes Per Day", title = "Minutes of Exercise", color = "Age Group") +
    scale_color_manual(labels = c("15-19", "20-64", "65+"), values = c("#E69F00", "#56B4E9", "#009E73")) +
    theme(text = element_text(size = 20))

pdf(paste0(map.out.dir, "exercise.pdf"))
plot(exerciseplot)
dev.off()

# Line graph for minutes spent watching TV
watching.tv = actSum %>%
    select(c("TUYEAR","agegrp", "t120303", "t120304")) %>%
    mutate(tv.minutes = t120303 + t120304) %>%
    select(-c(t120303, t120304)) %>%
    dcast(TUYEAR + agegrp ~ ., mean) %>%
    setNames(c("year", "agegrp", "tv.minutes"))

video.games = actSum %>%
    select(c("TUYEAR","agegrp", "t120308")) %>%
    dcast(TUYEAR + agegrp ~ ., mean) %>%
    setNames(c("year", "agegrp", "video.game.minutes"))

screen.time = cbind(watching.tv, video.games) %>%
    melt(id.vars = c("year", "agegrp"), variable.name = "screenActivity",value.name = "MinPerDay")
screen.time$screenActivity = recode(screen.time$screenActivity, "'tv.minutes' = 'TV Minutes'; 'video.game.minutes' = 'Video Game Minutes'")

screenTimePlot = ggplot(data = screen.time, aes(x = year, y = MinPerDay, color = as.factor(agegrp), shape =  as.factor(screenActivity)), group = interaction(as.factor(agegrp), as.factor(screenActivity))) +
    geom_line() + geom_point(size = 4) +
    labs(y = "Minutes Per Day", x = "Year", title = "Screen Time", color = "Age Group") +
    scale_color_manual(labels = c("15-19", "20-64", "65+"), values = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00")) +
    theme(text = element_text(size = 20)) +
    guides(shape=guide_legend(title="Screen Time"))

pdf(paste0(map.out.dir, "screen time.pdf"))
plot(screenTimePlot)
dev.off()
