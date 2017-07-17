library(ggplot2)
library(mice)
library(dplyr)
library(mi)
library(lattice)

#Loading the R.Data file
#Imputed data
fairfax_imputed <- load("./data/comm_fairfax/working/miceoutput3_power_transform.Rdata")
#Original data file
fairfax_original <- rio::import("~/git/comm_fairfax/data/comm_fairfax/working/pums_person_interest.csv")

class(mice.out)
#get the imputed data set out
fairfax_imputed_final_ver_2 <- mice::complete(mice.out)
write.csv(fairfax_imputed_final_ver_2, file = "~/git/comm_fairfax/data/comm_fairfax/working/fairfax_imputed_power_transform.csv")
sapply(fairfax_imputed_final_ver_2, function(x) sum(is.na(x)))
md.pattern(fairfax_imputed_final_ver_2)
fairfax_imputed_final <- fairfax_imputed_final_ver_2

#################################
# BUILT IN FUNCTIONS ARE USELESS
#################################
#FIRST DIAGNOSTIC WITH STRIPPLOT (Takes a long time execute!)
#Will run over night
#stripplot(mice.out, pch = 2, cex = 0.5)

#DRAW CONVERGE GRAPHS#
#Something is not working out. It's only drawing ENG
#plot(fairfax_imputed_final)
#DENSITY PLOTS
#barplot(fairfax_imputed_final$RAC1P)

#WHAT SORT OF RELATIONSHIP GRAPHS SHOULD I DRAW


##################
#MY OWN COMPARISON
##################
#COMPARING DISTRIBUTION OF RACE VALUES
par(mfrow=c(1,2))
barplot(table(fairfax_imputed_final$RAC1P), main = "Imputed Race")
barplot(table(fairfax_original$RAC1P), main = "Original Race")
title("Imputed vs Original Distribution for Race", outer = TRUE)

#COMPARING DISTRIBUTION OF SEX
par(mfrow=c(1,2))
barplot(table(fairfax_imputed_final$SEX), main = "Imputed Sex")
barplot(table(fairfax_original$SEX), main = "Original Sex")

#COMPARING DISTRIBUTION OF AGE # PROBLEM # MAKE SURE AGE CANNOT BE LESS THAN 0 or MORE THAN 100. Maybe use PMM.
par(mfrow=c(1,2))
plot(density(fairfax_imputed_final$AGEP), main = "Imputed Age")
plot(density(fairfax_original$AGEP), main = "Original Age")

par(mfrow=c(1,2))
plot(density(fairfax_imputed_final$AGEP ** (3/2)), main = "Imputed Age")
plot(density(fairfax_original$AGEP ** (3/2)), main = "Original Age")



#COMPARING DISTRIBUTION OF Cognitive difficulty
par(mfrow=c(1,2))
barplot(table(fairfax_imputed_final$DREM), main = "Imputed Cognitive Difficulty")
barplot(table(fairfax_original$DREM), main = "Original Cognitivie Difficulty")

#COMPARING DISTRIBUTION OF TOTAL PERSON's INCOME# MAKE SURE INCOME CANNOT BE LESS THAN 0
#Question mark here???
par(mfrow=c(1,2))
plot(density(fairfax_imputed_final$PINCP ** (3)), main = "Imputed Income")
plot(density(fairfax_original$PINCP), main = "Original Income")

par(mfrow=c(1,2))
plot(density(fairfax_imputed_final$PINCP), main = "Imputed Income")
plot(density(fairfax_original$PINCP), main = "Original Income")



#COMPARING DISTRIBUTION OF PUBLIC ASSISTANCE INCOME
#EXAMINE THIS DATA MORE
par(mfrow=c(1,2))
barplot(table(fairfax_imputed_final$PAP), main = "Imputed P.Assistance Income", xlim = c(-200,200))
barplot(table(fairfax_original$PAP), main = "Original P.Assistance Income", xlim = c(-200,200))

#COMPARING DISTRIBUTION OF ENGLISH LANGUAGE SPEAKER
par(mfrow=c(1,2))
barplot(table(fairfax_imputed_final$ENG), main = "Imputed English Language")
barplot(table(fairfax_original$ENG), main = "Original English Language")
##########################
#Experimental Pairs
pairs(fairfax_original[,2:8])
pairs(fairfax_imputed_final)

#Age and Income: Turns them into Come
#The way that ACS has cut them.

#Predictive Mean Matching






