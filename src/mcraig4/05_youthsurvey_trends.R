library(rio)
library(plyr)
library(ggplot2)
library(dplyr)

youth_survey_16 <- import('data/comm_fairfax/youth_survey/working/2016_8_10_12_Youth_Survey_formatted.csv')
youth_survey_15 <- import('data/comm_fairfax/youth_survey/working/2015_8_10_12_Youth_Survey_formatted.csv')

fruit_16 <- data.frame(table(youth_survey_16$H5))
salad_16 <- data.frame(table(youth_survey_16$H16))
potatoes_16 <- data.frame(table(youth_survey_16$H17))
carrots_16 <- data.frame(table(youth_survey_16$H18))
other_veg_16 <- data.frame(table(youth_survey_16$H19))

colnames(fruit_16)[2] <- 'fruit_freq'
colnames(salad_16)[2] <- 'salad_freq'
colnames(potatoes_16)[2] <- 'potato_freq'
colnames(carrots_16)[2] <- 'carrot_freq'
colnames(other_veg_16)[2] <- 'other_veg_freq'

levels(fruit_16$Var1) <- c(levels(fruit_16$Var1), "Not eaten")
levels(salad_16$Var1) <- c(levels(salad_16$Var1), "Not eaten")
levels(potatoes_16$Var1) <- c(levels(potatoes_16$Var1), "Not eaten")
levels(carrots_16$Var1) <- c(levels(carrots_16$Var1), "Not eaten")
levels(other_veg_16$Var1) <- c(levels(other_veg_16$Var1), "Not eaten")

fruit_16$Var1[7] = 'Not eaten'
salad_16$Var1[7] = 'Not eaten'
potatoes_16$Var1[7] = 'Not eaten'
carrots_16$Var1[7] = 'Not eaten'
other_veg_16$Var1[7] = 'Not eaten'


dfs_16 <- list(
    fruit_16,
    salad_16,
    potatoes_16,
    carrots_16,
    other_veg_16
)

nutritious_16 <- join_all(dfs_16, by = 'Var1', type = 'left', match = 'first')

nutritious_16$total <- rowSums(nutritious_16[2:6])
nutritious_16$year <- 2016
total_16 <- nutritious_16[, c(1,7,8)]
x <- c("1 time per day", "2 times per day ", "3 times per day ",
       "4 or more times per day","1 to 3 times during the past 7 days ",
       "4 to 6 times during the past 7 days ", "Not eaten")

total_16 <- data.frame(total_16 %>%
                           slice(match(x, Var1)))


fruit_15 <- data.frame(table(youth_survey_15$H5))
salad_15 <- data.frame(table(youth_survey_15$H16))
potatoes_15 <- data.frame(table(youth_survey_15$H17))
carrots_15 <- data.frame(table(youth_survey_15$H18))
other_veg_15 <- data.frame(table(youth_survey_15$H19))

colnames(fruit_15)[2] <- 'fruit_freq'
colnames(salad_15)[2] <- 'salad_freq'
colnames(potatoes_15)[2] <- 'potato_freq'
colnames(carrots_15)[2] <- 'carrot_freq'
colnames(other_veg_15)[2] <- 'other_veg_freq'

levels(fruit_15$Var1) <- c(levels(fruit_15$Var1), "Not eaten")
levels(salad_15$Var1) <- c(levels(salad_15$Var1), "Not eaten")
levels(potatoes_15$Var1) <- c(levels(potatoes_15$Var1), "Not eaten")
levels(carrots_15$Var1) <- c(levels(carrots_15$Var1), "Not eaten")
levels(other_veg_15$Var1) <- c(levels(other_veg_15$Var1), "Not eaten")

fruit_15$Var1[7] = 'Not eaten'
salad_15$Var1[7] = 'Not eaten'
potatoes_15$Var1[7] = 'Not eaten'
carrots_15$Var1[7] = 'Not eaten'
other_veg_15$Var1[7] = 'Not eaten'


dfs_15 <- list(
    fruit_15,
    salad_15,
    potatoes_15,
    carrots_15,
    other_veg_15
)

nutritious_15 <- join_all(dfs_15, by = 'Var1', type = 'left', match = 'first')

nutritious_15$total <- rowSums(nutritious_15[2:6])
nutritious_15$year <- 2015
total_15 <- nutritious_15[, c(1,7,8)]
x <- c("1 time per day", "2 times per day ", "3 times per day ",
       "4 or more times per day","1 to 3 times during the past 7 days ",
       "4 to 6 times during the past 7 days ", "Not eaten")

total_15 <- data.frame(total_15 %>%
                        slice(match(x, Var1)))


total <- rbind(total_15, total_16)

ggplot(data = total, aes(factor(Var1), total, fill = as.factor(year))) +
    geom_bar(stat = 'identity', width = 0.4, position = "dodge")
