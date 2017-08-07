library(ggplot2)
library(mice)
library(dplyr)
library(mi)
library(MASS)
library(normalr)
#library(caret)
#library(car)

#Clear everything in the environment
rm(list = ls())
#Import the interested pums_person_interest
pums_person_interest <- rio::import("~/git/comm_fairfax/data/comm_fairfax/working/PUMS_person_interest.csv")
pums_person_interest <- pums_person_interest[, 2:ncol(pums_person_interest)]
nrow(pums_person_interest)

fairfax_pop <- rio::import("~/git/comm_fairfax/data/comm_fairfax/working/fairfax_pop.csv")

ncol(pums_person_interest)
#Completely present columns:

typeof(pums_person_interest$RACE1P)
typeof(pums_person_interest$SEX)
typeof(pums_person_interest$AGEP)
typeof(pums_person_interest$DREM)

#RACE1P : Recorded Detail Race Code, Unordered multinomial Variable
#SEX : Sex, Unordered Binary variable
#AGEP : Age, Make this a continuous variable
#Missing Variables
#DREM : Cognitive difficulty, N/A refers to less than 5 years old
#PINCP:Income in the past 12 months, N/A refers to less than 15 years old
#PAP: Public Assistance Income in the past 12 months, N/A public assistance income
#ENG: Ability to speak English, N/A Less than 5 years old/speak only English, Unordered multinomial Variable
md.pattern(pums_person_interest)

#IMPUTE THE NAs of the respective PUMS_PERSON_INTEREST with the needed variables
#PINCP # Check the distribution
length(which(is.na(pums_person_interest$PINCP))) #13926 has NA values. That means they are younger than 15. We are going impute $0 to them
summary(pums_person_interest$PINCP)
length(sum(pums_person_interest$PINCP < 0)) # Only 1 is less than 0
pums_person_interest$PINCP[pums_person_interest$PINCP < 0] <- 0
pums_person_interest$PINCP[which(is.na(pums_person_interest$PINCP))] <- 0 #Making all the NAs become zero

#Left Skewed. #Predictive Mean Modeling might be best. No matter what I impute, it's getting bi-model
#Or right skewed!
# plot(density(pums_person_interest$PINCP))
# plot(density(sqrt(pums_person_interest$PINCP)))
# plot(density(pums_person_interest$PINCP ** (1/3)) )
# plot(density(pums_person_interest$PINCP ** (1/4)) )
# plot(density(pums_person_interest$PINCP ** (1/100)) )

#pums_person_interest$PINCP <- pums_person_interest$PINCP ** (1/3)
#Changing personal income to factor variable
pums_person_interest$PINCP[pums_person_interest$PINCP >= 0 & pums_person_interest$PINCP <= 9999 ] <- 1
pums_person_interest$PINCP[pums_person_interest$PINCP >= 10000 & pums_person_interest$PINCP <= 14999 ] <- 2
pums_person_interest$PINCP[pums_person_interest$PINCP >= 15000 & pums_person_interest$PINCP <= 24999 ] <- 3
pums_person_interest$PINCP[pums_person_interest$PINCP >= 25000 & pums_person_interest$PINCP <= 34999 ] <- 4
pums_person_interest$PINCP[pums_person_interest$PINCP >= 35000 & pums_person_interest$PINCP <= 49999 ] <- 5
pums_person_interest$PINCP[pums_person_interest$PINCP >= 50000 & pums_person_interest$PINCP <= 64999 ] <- 6
pums_person_interest$PINCP[pums_person_interest$PINCP >= 65000 & pums_person_interest$PINCP <= 74999 ] <- 7
pums_person_interest$PINCP[pums_person_interest$PINCP >= 75000] <- 8
unique(pums_person_interest$PINCP)

#Changing everything into factor
pums_person_interest$PINCP <- as.factor(pums_person_interest$PINCP)

#Test my assumptions
# test_income <- pums_person_interest$PINCP
# length(test_income[test_income == 0]) #22381
# length(test_income[test_income > 0]) #61091
# test_income_not_zero <- test_income[test_income > 0]
# plot(density(test_income_not_zero ** (1/5)) )
# summary(test_income_not_zero)
# table(test_income_not_zero)
# summary(test_income_not_zero)
# length(test_income_not_zero[test_income_not_zero > 500000]) #106 super wealthy
# test_income_not_zero_notswealthy <- test_income_not_zero[!test_income_not_zero > 150000]
# test_income_middle <- test_income_not_zero_notswealthy[!test_income_not_zero_notswealthy < 20000]
# plot(density(test_income_middle))
#


#Do Transformation Here. We are going to keep PINCP transformation in Log format.
#pums_person_interest$PINCP <- log(pums_person_interest$PINCP)

#Some additional explorations
# length(pums_person_interest$PINCP[pums_person_interest$PINCP < 200])
# length(pums_person_interest$PINCP)
# test <- (pums_person_interest$PINCP[pums_person_interest$PINCP > 200])
# plot(density(test))
# summary(test)
# length(pums_person_interest$PINCP[pums_person_interest$PINCP > 100000])
# length(pums_person_interest$PINCP)


#DREM
length(which(is.na(pums_person_interest$DREM))) #4230 has NA values. N/A refers to less than 5 years old. We are imputing value 3 to it.
unique(pums_person_interest$DREM) # 2 NA 1
pums_person_interest$DREM[which(is.na(pums_person_interest$DREM))] <- 3
unique(pums_person_interest$DREM)

#ENG
length(which(is.na(pums_person_interest$ENG))) #72741 has NA values. N/A refers to less than 5 years old or speaks only English: We will impute it by 5
pums_person_interest$ENG[which(is.na(pums_person_interest$ENG))] <- 5
unique(pums_person_interest$ENG)

#PAP
#Cap this at 2,000 and do log transform
#Check if it's normal
length(which(is.na(pums_person_interest$PAP))) #NA refers to less than 15 years old
summary(pums_person_interest$PAP) #PMM Match. Switch. Perfect!
pums_person_interest$PAP[pums_person_interest$PAP > 2000] <- 2000
pums_person_interest$PAP[which(is.na(pums_person_interest$PAP))] <- 0 #Ma
#plot(density(pums_person_interest$PAP))
#test2 <- log(pums_person_interest$PAP)
#plot(density(test2))
#Convert PAP to binary variable
pums_person_interest$PAP[pums_person_interest$PAP > 0] <- 2
pums_person_interest$PAP[pums_person_interest$PAP <= 0] <- 1

#Convert it to a factor
pums_person_interest$PAP <- as.factor(pums_person_interest$PAP)

#RACE : NO NAs. Everything is good
length(which(is.na(pums_person_interest$RAC1P)))

#SEX : NO NAs. Everything is good
length(which(is.na(pums_person_interest$SEX)))

#AGEP : NO NAs. Everything is good
#Do the PMM matching. Looking at the distribution first and see what's going on?
# length(which(is.na(pums_person_interest$AGEP))) #AGEP
# plot(density(log(pums_person_interest$AGEP)))
# plot(density(pums_person_interest$AGEP))
# plot(density(pums_person_interest$AGEP ** (1/(1.5))))
# plot(density(sqrt(pums_person_interest$AGEP)))
# plot(density(pums_person_interest$AGEP ** (1/3)))
# plot(density(pums_person_interest$AGEP ** (1/4)))


#Transform
#pums_person_interest$AGEP <- pums_person_interest$AGEP ** (1/(1.5))

#Age is also going to be a categorical variable
pums_person_interest$AGEP[pums_person_interest$AGEP >= 0 & pums_person_interest$AGEP < 25] <- 1
pums_person_interest$AGEP[pums_person_interest$AGEP >= 25 & pums_person_interest$AGEP < 50] <- 2
pums_person_interest$AGEP[pums_person_interest$AGEP >= 50 & pums_person_interest$AGEP < 75] <- 3
pums_person_interest$AGEP[pums_person_interest$AGEP >= 75] <- 4
unique(pums_person_interest$AGEP)

#Check if all has been taken care of for missing
md.pattern(pums_person_interest)

#Write it out
write.csv(pums_person_interest, file = "~/git/comm_fairfax/data/comm_fairfax/working/pums_person_interest_factors.csv")

#Now, we need to combine it with the rest of empty over 1 million rows and run the initial plots.

#Pulling in LatLong included empty data frame to fit.
fairfax_all_latlong <- rio::import("~/git/comm_fairfax/data/comm_fairfax/final/fairfax_latlong.csv")
names(fairfax_all_latlong)
fairfax_all_latlong <- fairfax_all_latlong[-1]
#Pulling in pums_person_interest_factors
pums_person_interest <- rio::import("~/git/comm_fairfax/data/comm_fairfax/working/pums_person_interest_factors.csv")


pums_person_interest$ZCTAS <- NA
pums_person_interest$long <- NA
pums_person_interest$lat <- NA
pums_person_interest$HighSchool <- NA
pums_person_interest <- pums_person_interest[-1]
names(pums_person_interest)

pums_combined <- rbind(pums_person_interest, fairfax_all_latlong)
names(pums_combined)


#FIXING THE NAS IN ALL THE ROWS
#CHECK HOW MANY NAS FOR EACH ROW AND EXPLAIN WHY YOU IMPUTE IT THAT WAY
s
#Making sure Variables that need to be factors are factors
pums_combined$RAC1P <- as.factor(pums_combined$RAC1P)
pums_combined$SEX <- as.factor(pums_combined$SEX)
pums_combined$DREM <- as.factor(pums_combined$DREM)
pums_combined$ENG <- as.factor(pums_combined$ENG)
pums_combined$PAP <- as.factor(pums_combined$PAP)
pums_combined$PINCP <- as.factor(pums_combined$PINCP)
pums_combined$AGEP <- as.factor(pums_combined$AGEP)

write.csv(pums_combined, file = "~/git/comm_fairfax/data/comm_fairfax/working/pums_combined_factor.csv")
unique(pums_combined$RAC1P)


####################
#REFER TO SCRIPT UNDER FOLDER: /home/zarni90/git/comm_fairfax
####################
#Put Seed in
#maxit --: Number of iterations (Default is given as 5)
#What is a good iterations number?
#START FROM HERE!!!
pums_combined <- rio::import("~/git/comm_fairfax/data/comm_fairfax/working/pums_combined_factor.csv")
names(pums_combined)
pums_combined <- pums_combined[-1]
ncol(pums_combined)
names(pums_combined)
unique(pums_combined$DREM)
class(pums_combined$PAP)

pums_combined$RAC1P <- as.factor(pums_combined$RAC1P)
pums_combined$SEX <- as.factor(pums_combined$SEX)
pums_combined$DREM <- as.factor(pums_combined$DREM)
pums_combined$ENG <- as.factor(pums_combined$ENG)
pums_combined$PAP <- as.factor(pums_combined$PAP)

unique(pums_combined$ENG)

numdraws <- 10
niter <- 10
mice.out <- mice(data=pums_combined%>% dplyr::select(RAC1P,SEX,AGEP,DREM,PINCP, PAP, ENG), m=numdraws,maxit = niter,
                 method=c("polyreg","logreg","polyreg","polyreg","polyreg", "logreg", "polyreg"), seed = 1234)

#ENDS HERE


#Diagonostic Code








