library(car)
library(randomForest)
library(dplyr)
# Read in youth survey data
ffxys6 = read.csv("../comm_fairfax/data/comm_fairfax/original/youth survey data/2016_6_Youth_Survey.csv")
ffxys8 = read.csv("../comm_fairfax/data/comm_fairfax/original/youth survey data/2016_8_10_12_Youth_Survey.csv")
shared.vars = read.csv("../comm_fairfax/data/comm_fairfax/original/youth survey data/sharedvariables.csv", stringsAsFactors = F)

# Recode I1 and I2
# Ask about school for 6th graders
ffxys6$I1 = car::recode(ffxys6$I1, "1 = 10; 2 = 11; 3 = 12; 4 = 13; 5 = 14")
ffxys6$I2 = car::recode(ffxys6$I2, "1 = '5th';2 = '6th'; 3 = '7th'")
ffxys8$I1 = car::recode(ffxys8$I1, "1 = 10; 2 = 11; 3 = 12; 4 = 13; 5 = 14; 6 = 15; 7 = 16; 8 = 17; 9 = 18; 10 = 19")
ffxys8$I2 = car::recode(ffxys8$I2, "1 = '8th';2 = '10th'; 3 = '12th'")

# The I6 series has a lot of missing, which in this case means 'the person in question does not live with me.' Recoding those to
ffxys6[,grepl("I6", colnames(ffxys6))][is.na(ffxys6[,grepl("I6", colnames(ffxys6))])] = 0
ffxys8[,grepl("I6", colnames(ffxys8))][is.na(ffxys8[,grepl("I6", colnames(ffxys8))])] = 0

shared.cols = sort(intersect(colnames(ffxys6),colnames(ffxys8)))
ffxys = rbind(ffxys6[,shared.cols], ffxys8[, shared.cols])
rm(ffxys6, ffxys8)

# The response is H6, minutes spent exercising
ffxys  = ffxys %>% subset(!is.na(ffxys$H3)) %>% select(-wgt)
ffxys = ffxys[!apply(ffxys, 1, anyNA),]
ffxys = ffxys %>% mutate_all(as.factor)
rows = sample(1:nrow(ffxys), 5000)
y = recode(ffxys$H3[rows], c("1 = 1;  2:7 = 2; 8 = 3"),as.factor.result = T)

system.time(ff.rf <- randomForest(y, x = ffxys[rows,-grep("H3", colnames(ffxys))],ntree = 500, mtry = 10))
ff.rf
plot(ff.rf)

y = as.numeric(y)
X = ffxys[rows,-24]
dat = cbind(y, X)
system.time(ff.lm <- lm(y ~ ., data = dat))
summary(ff.lm)
plot(ff.lm)

cv.test = function(x,y) {
    CV = sqrt(chisq.test(x, y, correct=FALSE)$statistic /
                  (length(x) * (min(length(unique(x)),length(unique(y))) - 1)))
    print.noquote("Cramér V / Phi:")
    return(as.numeric(CV))
}

chisq.mat = matrix(0, 85, 85)
diag(chisq.mat) = 0
for(i in 1:84){
    for(j in (i+1):85){
        chisq.mat[i, j] = chisq.mat[j, i] = cv.test(ffxys[,i], ffxys[,j])
        print(i)
    }

}
plot(cmdscale(chisq.mat))
#27, 28, 73, 18

ggplot

