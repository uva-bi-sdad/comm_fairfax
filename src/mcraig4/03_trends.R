library(rio)
library(ggplot2)

BMI <- import('data/comm_fairfax/working/ATUS_data_working/ATUS state year files/Avg_BMI.csv')
head(BMI)
tail(BMI)


for (i in 2003:2016) {
    year_avg <- sum(BMI$value)
    print(year_avg)
}

for (i in 2003:2016) {
    avg[i] <- mean(BMI$value[i])
    print(avg[i])
}

if BMI$year == 2003
