ys_older_2016 <- read.csv('data/comm_fairfax/original/youth survey data/2016_8_10_12_Youth_Survey.csv')
ys_younger_2016 <- read.csv('data/comm_fairfax/original/youth survey data/2016_6_Youth_Survey.csv')

# Subset the data to get the different variables (C11,12,13,14 vs H1,2 vs H20)
# Sleep isn't included in the 6th grade survey
older_c_var <- ys_older_2016[, 38:41]
older_h_var <- ys_older_2016[, 33:34]
older_sleep_var <- ys_older_2016[, 159:160]
younger_c_var <- ys_younger_2016[, 44:47]
younger_h_var <- ys_younger_2016[, 48:49]

# Recode the values
older_c_var[older_c_var == 1] <- 0
older_c_var[older_c_var == 2] <- 0.5
older_c_var[older_c_var == 3] <- 0.75
older_c_var[older_c_var == 4] <- 1
older_c_var[older_c_var == 5] <- 2
older_c_var[older_c_var == 6] <- 3

older_h_var[older_h_var == 1] <- 0
older_h_var[older_h_var == 2] <- 0.75
older_h_var[older_h_var == 3] <- 1
older_h_var[older_h_var == 4] <- 2
older_h_var[older_h_var == 5] <- 3
older_h_var[older_h_var == 6] <- 4
older_h_var[older_h_var == 7] <- 5

# This isn't write, due to recoding 1 to 4 then 4 to 7 and so on
# Look into car package?
older_sleep_var[older_sleep_var == 1] <- 4
older_sleep_var[older_sleep_var == 2] <- 5
older_sleep_var[older_sleep_var == 3] <- 6
older_sleep_var[older_sleep_var == 4] <- 7
older_sleep_var[older_sleep_var == 5] <- 8
older_sleep_var[older_sleep_var == 6] <- 9
older_sleep_var[older_sleep_var == 7] <- 10

# Cbind all the above dataframes
older_activity_var <- cbind(older_c_var, older_h_var)

# Recode similar to above steps
younger_c_var[younger_c_var == 1] <- 0
younger_c_var[younger_c_var == 2] <- 0.5
younger_c_var[younger_c_var == 3] <- 0.75
younger_c_var[younger_c_var == 4] <- 1
younger_c_var[younger_c_var == 5] <- 2
younger_c_var[younger_c_var == 6] <- 3

younger_h_var[younger_h_var == 1] <- 0
younger_h_var[younger_h_var == 2] <- 0.45
younger_h_var[younger_h_var == 3] <- 1
younger_h_var[younger_h_var == 4] <- 2
younger_h_var[younger_h_var == 5] <- 3
younger_h_var[younger_h_var == 6] <- 4
younger_h_var[younger_h_var == 7] <- 5

# Cbind all the above dataframes
younger_activity_var <- cbind(younger_c_var, younger_h_var)

# Get the mean and standard deviation for both the 8_10_12 grade files and the 6 grade files
colMeans(older_activity_var, na.rm = TRUE)
sapply(older_activity_var, sd, na.rm = TRUE)

colMeans(younger_activity_var, na.rm = TRUE)
sapply(younger_activity_var, sd, na.rm = TRUE)




## Want to learn how to automate this process
# df <- {}
# for (i in 1:6) {
#     older_c_var[older_c_var == i] <- c(0, 0.5, 0.75, 1, 2, 3)
#
# }
