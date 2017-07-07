library(ggplot2)
library(mice)
library(dplyr)
library(mi)


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

#Creating the empty data frame for all the missing people
pums_all_fairfax <- data.frame(matrix(NA, ncol = 7, nrow = 1081726))
names(pums_all_fairfax) <- names(pums_person_interest)
ncol(pums_all_fairfax)
ncol(pums_person_interest)
#Adding zip codes by the number of its population
fairfax_pop <- fairfax_pop[-1]
zip_vectir <- rep(fairfax_pop$ZCTA5, times = fairfax_pop$POPPT)
#Adding to Fairfax data frame
pums_all_fairfax$ZCTAS <- zip_vectir
write.csv(pums_all_fairfax, file = "~/git/comm_fairfax/data/comm_fairfax/working/pums_all_fairfax.csv")
View(pums_all_fairfax)
pums_person_interest$ZCTAS <- NA

pums_combined <- rbind(pums_person_interest, pums_all_fairfax)
names(pums_combined)
write.csv(pums_combined, file = "~/git/comm_fairfax/data/comm_fairfax/working/pums_combined.csv")
numdraws <- 10



#PINCP is income
pums_all_fairfax[pums_all_fairfax$PINCP < 0,]



pums_combined$RAC1P <- as.factor(pums_combined$RAC1P)
pums_combined$SEX <- as.factor(pums_combined$SEX)
pums_combined$DREM <- as.factor(pums_combined$DREM)
pums_combined$PAP <- as.factor(pums_combined$PAP)
pums_combined$ENG <- as.factor(pums_combined$ENG)





#Put Seed in
#maxit --: Number of iterations (Default is given as 5)
#What is a good iterations number?


mice.out <- mice(data=pums_combined%>% dplyr::select(RAC1P,SEX,AGEP,DREM,PINCP, PAP, ENG), m=numdraws,
                 method=c("polyreg","logreg","norm","logreg","norm", "polyreg", "polyreg"))




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





