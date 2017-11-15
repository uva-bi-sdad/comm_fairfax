library(ggplot2)
library(reshape)
library(dplyr)

# Want trends for each major category (trend == time series plot) for each age group as well

#statefips = read.csv("./data/comm_fairfax/original/ATUS_Data/statefips.csv", header = T, stringsAsFactors = F)
#actSum = read.csv("./data/comm_fairfax/working/ATUS_data_working/ATUSsummaryAgeState.csv")

tokens = unique(substr(colnames(actSum)[28:451], 2, 3))
ncat = length(tokens)
category.list = matrix(NA, nrow(actSum), ncat + 1)

colnames(category.list) = c("personal_care", "household_act", "helping_HH_mem", "helping_nHH_mem", "work_act", "education", "consum_purc", "prof_pers_care", "house_serv", "civic_obli", "eat_drink", "social_relax", "sports_rec", "religious", "volunteer", "telephone", "travel", "exercise_summary")

for(i in 1:ncat){
    # Find the columns with a particular token
    category.list[,i] = rowSums(actSum[,which(substr(colnames(actSum), 2, 3) == tokens[i])])
}
exercise.cols = c("t030105", "t040105", "t050203", "t150301", grep("1301", colnames(actSum), value = T))
exercise.cols = exercise.cols[-c(9, 16, 18, 22, 38, 42)]
category.list[,ncat + 1] = rowSums(actSum[,exercise.cols])
category.list = data.frame(TUYEAR = actSum$TUYEAR, agegrp = actSum$agegrp, category.list)
category.list = melt(category.list, id.vars = c("TUYEAR", "agegrp"), var = "ATUS.Category")
dataYearByAge = with(category.list, tapply(value, list(TUYEAR, agegrp, ATUS.Category), mean))
dataYearByAge = melt(dataYearByAge, varnames = c("year", "agegrp", "ATUScat"))

write.csv(dataYearByAge, "./data/comm_fairfax/working/ATUS trend summary data.csv")

# This file is then read into ATUS trends.Rmd, which contains the code to make the trend plots. I don't think I like this setup very much, but it's the best I could come up with at the moment. Eventually I'd like to pull that code back into the script and maybe knit it togethr in here.

# Here I'll investigate some high proportion of 0 responses

unique(actSum$TEAGE)

workdays = which(actSum$TUDIARYDAY %in% 2:6)
cols = actSum[workdays, which(substr(colnames(actSum), 2, 3) == tokens[5])]

zero.prop = tapply(rowSums(cols), actSum$TEAGE[workdays], function(x) sum(x == 0)/length(x))
plot(sort(unique(actSum$TEAGE)), zero.prop, ylim = c(0, 1))

# Many 0's is the norm, my analysis should reflect this.


cat.names = unique(trend.data$ATUScat)

zero.prop = with(category.list, tapply(value, list(TUYEAR, agegrp, ATUS.Category), function(x) sum(x == 0)/length(x)))
zero.prop = melt(zero.prop, varnames = c("year", "agegrp", "ATUScat"))

positive.mean = with(category.list, tapply(value, list(TUYEAR, agegrp, ATUS.Category), function(x) mean(x[x > 0])))
positive.mean = melt(positive.mean, varnames = c("year", "agegrp", "ATUScat"))

# Can run this loop with dataYearByAge, zero.prop, or positive.mean in the first argument for subset.
#dataYearByAge
#positive.mean
#zero.prop

# This needs work, it's just sloppy. Might list these data objects, or make another data frame with another column to indicate which table. That feels right. idk.

for(i in 1:length(cat.names)){

  active.data = subset(positive.mean, ATUScat == cat.names[i])

  plot = ggplot(data = active.data, aes(x = year, y = value, color = as.factor(agegrp))) +
    geom_line() + geom_point() +
    labs(y = "Minutes Per Day", title = paste0("ATUS Category: ", cat.names[i], " overall mean."), color = "Age Group") +
    scale_color_manual(labels = c("15-19", "20-64", "65+"), values = rainbow(3))
  plot(plot)
}





