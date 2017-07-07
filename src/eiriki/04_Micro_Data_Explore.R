library(data.table)
library(rio)
#this is a script that will make cross tabs for the youth survey data
micro <- import('data/comm_fairfax/original/youth_survey/youth_survey_2016_formatted.csv')
depress_table <- xtabs(~I2 + I3 + race_eth + M1, data = micro)
depress_df <- as.data.table(depress_table)
