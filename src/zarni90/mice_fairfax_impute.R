library(ggplot2)
library(mice)
library(dplyr)
library(mi)

#Clear everything in the environment
rm(list = ls())
#Import the interested pums_person_interest
pums_person_interest <- rio::import("~/git/comm_fairfax/data/comm_fairfax/working/PUMS_person_interest.csv")
pums_person_interest <- pums_person_interest[, 2:ncol(pums_person_interest)]
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
#PINCP
length(which(is.na(pums_person_interest$PINCP))) #13926 has NA values. That means they are younger than 15. We are going impute $0 to them
pums_person_interest$PINCP[which(is.na(pums_person_interest$PINCP))] <- 0

#DREM
length(which(is.na(pums_person_interest$DREM))) #4230 has NA values. N/A refers to less than 5 years old. We are imputing value 3 to it.
unique(pums_person_interest$DREM) # 2 NA 1
pums_person_interest$DREM[which(is.na(pums_person_interest$DREM))] <- 3

#ENG
length(which(is.na(pums_person_interest$ENG))) #72741 has NA values. N/A refers to less than 5 years old or speaks only English: We will impute it by 5
pums_person_interest$ENG[which(is.na(pums_person_interest$ENG))] <- 5

#PAP
length(which(is.na(pums_person_interest$PAP))) #NA refers to less than 15 years old
pums_person_interest$PAP[which(is.na(pums_person_interest$PAP))] <- 0 #Ma

#RACE : NO NAs. Everything is good
length(which(is.na(pums_person_interest$RAC1P)))

#SEX : NO NAs. Everything is good
length(which(is.na(pums_person_interest$SEX)))

#AGEP : NO NAs. Everything is good
length(which(is.na(pums_person_interest$AGEP))) #AGEP
md.pattern(pums_person_interest)
write.csv(pums_person_interest, file = "~/git/comm_fairfax/data/comm_fairfax/working/pums_person_interest.csv")

#Now, we need to combine it with the rest of empty over 1 million rows and run the initial plots.


#Creating the empty data frame for all the missing people
pums_all_fairfax <- data.frame(matrix(NA, ncol = 7, nrow = 1081726))
#Appending column names to it
names(pums_all_fairfax) <- names(pums_person_interest)
#Number of columns
ncol(pums_all_fairfax)
#Number of pums_person_column
ncol(pums_person_interest)
#Adding zip codes by the number of its population
#Removing the first column of the ZCTA by population thing
fairfax_pop <- fairfax_pop[-1]
#Repeating the numbers of ZCTAs by population
zip_vectir <- rep(fairfax_pop$ZCTA5, times = fairfax_pop$POPPT)
pums_all_fairfax$ZCTAS <- zip_vectir
#Adding ZCTAs to Fairfax data frame

#Write this out for JOSH
#write.csv(pums_all_fairfax, file = "~/git/comm_fairfax/data/comm_fairfax/working/pums_all_fairfax.csv")
#pums_all_fairfax <- rio::import("~/git/comm_fairfax/data/comm_fairfax/working/pums_all_fairfax.csv")
#View(pums_all_fairfax)
#Making sure the pums_person file has the same number of columns
pums_person_interest$ZCTAS <- NA
ncol(pums_person_interest)
names(pums_person_interest)
ncol(pums_all_fairfax)
#pums_all_fairfax <- pums_all_fairfax[-1]
names(pums_all_fairfax)

pums_combined <- rbind(pums_person_interest, pums_all_fairfax)
names(pums_combined)
#write.csv(pums_combined, file = "~/git/comm_fairfax/data/comm_fairfax/working/pums_combined.csv")


#FIXING THE NAS IN ALL THE ROWS
#CHECK HOW MANY NAS FOR EACH ROW AND EXPLAIN WHY YOU IMPUTE IT THAT WAY

#Making sure Variables that need to be factors are factors
pums_combined$RAC1P <- as.factor(pums_combined$RAC1P)
pums_combined$SEX <- as.factor(pums_combined$SEX)
pums_combined$DREM <- as.factor(pums_combined$DREM)
pums_combined$ENG <- as.factor(pums_combined$ENG)





#Put Seed in
#maxit --: Number of iterations (Default is given as 5)
#What is a good iterations number?
#START FROM HERE!!!
pums_combined <- rio::import("~/git/comm_fairfax/data/comm_fairfax/working/pums_combined.csv")
names(pums_combined)
pums_combined <- pums_combined[-1]
ncol(pums_combined)
numdraws <- 5
niter <- 5
mice.out <- mice(data=pums_combined%>% dplyr::select(RAC1P,SEX,AGEP,DREM,PINCP, PAP, ENG), m=numdraws,maxit = niter,
                 method=c("polyreg","logreg","norm","polyreg","norm", "norm", "polyreg"), seed = 1234)

#ENDS HERE


#Diagonostic Code



#Following with nhanes from the tutorial
nhanes
md.pattern(nhanes)
md.pairs(nhanes)

imp <- mice(nhanes, seed = 23109)
print(imp)

#Imputations by each data set. We have a default data set of 5.
imp$imp$bmi

#Already pooled?
#It's not giving out all 5 imputed data sets?? Problem here! How do we pool this later on?
nhanes_pooled <- mice::complete(imp)
nhanes_first <- mice::complete(imp,2)
nrow(nhanes)





