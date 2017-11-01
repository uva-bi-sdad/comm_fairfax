library(car)


# Pull in the two survey datasets
ys_older_2016 <- read.csv('data/comm_fairfax/original/youth survey data/2016_8_10_12_Youth_Survey.csv')
ys_younger_2016 <- read.csv('data/comm_fairfax/original/youth survey data/2016_6_Youth_Survey.csv')


# Subset the data to get C11,12,13,14 and H1,2 and Sleep for eighth grade (H20)
older_c_var <- ys_older_2016[, 38:41]
older_h_var <- ys_older_2016[, 33:34]
older_sleep_var <- ys_older_2016[, 159:160]
younger_c_var <- ys_younger_2016[, 44:47]
younger_h_var <- ys_younger_2016[, 48:49]


# Using the car library (recode function), recode the values in an automated way
older_c_var <- data.frame(apply(older_c_var, 2, function(x) {x <- recode(x,"1 = 0; 2 = .5; 3 = .75; 4 = 1; 5 = 2; 6 = 3"); x}))
older_h_var <- data.frame(apply(older_h_var, 2, function(x) {x <- recode(x, "1=0;2=0.75;3=1;4=2;5=3;6=4;7=5"); x}))
older_sleep_var <- data.frame(apply(older_sleep_var, 2, function(x) {x <- recode(x,"1 = 4; 2 = 5; 3 = 6; 4 = 7; 5 = 8; 6 = 9; 7 = 10"); x}))
older_sleep_var <- data.frame(older_sleep_var[, 1])


# Cbind all the above dataframes
older_activity_var <- cbind(older_c_var, older_h_var, older_sleep_var)


# Recode the variables from the 6th grade survey
younger_c_var <- data.frame(apply(younger_c_var, 2, function(x) {x <- recode(x, "1=0;2=0.5;3=0.75;4=1;5=2;6=3"); x}))
younger_h_var <- data.frame(apply(younger_h_var, 2, function(x) {x <- recode(x, "1=0;2=0.75;3=1;4=2;5=3;6=4;7=5"); x}))


# Cbind the two above datasets
younger_activity_var <- cbind(younger_c_var, younger_h_var)


# Get the mean and standard deviation from the two above survey datasets
colMeans(older_activity_var, na.rm = TRUE)
sapply(older_activity_var, sd, na.rm = TRUE)

colMeans(younger_activity_var, na.rm = TRUE)
sapply(younger_activity_var, sd, na.rm = TRUE)
