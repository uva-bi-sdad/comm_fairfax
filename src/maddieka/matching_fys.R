# Fairfax youth survey
# Match variables and combine files for (1) 6 th graders and (2) 8-10- 12 th graders

library(readr)
library(dplyr)


sixth2015 <- read_csv("data/comm_fairfax/youth_survey/original/2015_6_Youth_Survey.csv")
eighth2015 <- read_csv("data/comm_fairfax/youth_survey/original/2015_8_10_12_Youth_Survey.csv")

intersect(names(sixth2015), names(eighth2015))

# recode 8-10-12 variables to match 6th grade survey ----

recoded81012 <- eighth2015

## recode 8-10-12 A2A
# A2A On how many occasions (if any) have you had beer, wine, or hard liquor in your lifetime?
# A2B Have you ever, even once in your lifetime, had more than a sip or two of beer, wine or hard liquor?
recoded81012$A2B <- NA
recoded81012$A2B[recoded81012$A2A == 1] <- 2 # No
recoded81012$A2B[recoded81012$A2A > 1] <- 1 # Yes

## recode 8-10-12 A3A
# A3A	On how many occasions (if any) have you had beer, wine, or hard liquor during the past 30 days?
# A3B	During the past 30 day on how many days did you drink beer, wine, or hard liquor?
recoded81012$A3B <- eighth2015$A3A
recoded81012$A3B[recoded81012$A3B > 4] <- 5 # 10+ times

## recode 8-10-12 D5,D7,D11,D13,D15,D17,D19,D21,D22,D26
# D5-26 On how many occasions have you used crack, LSD, meth, heroin, etc...
# D24	Have you ever, even once in your lifetime used other illegal drugs (not counting alcohol, tobacco or marijuana)?
drug_questions <- eighth2015[,c("D5","D7","D11","D13","D15","D17","D19","D21","D26")]
recoded81012$total <- rowSums(drug_questions)

recoded81012$D24 <- NA
recoded81012$D24[recoded81012$total > 9] <- 1 # Yes (tried at least 1)
recoded81012$D24[recoded81012$total == 9] <- 2 # No

## recode 8-10-12 D5,D7,D11,D13,D15,D17,D19,D21,D22,D26
# D5-26 On how many occasions have you used crack, LSD, meth, heroin, etc...
# D25	During the past 30 days on how many days have you used other illegal drugs (not counting alcohol, tobacco or marijuana)?
conversion <- data.frame(value = c(seq(1,7,1)), min = c(0,1,3,6,10,20,40), max = c(0,2,5,9,19,39,100))

drug_answers <- drug_questions
drug_answers[drug_answers==1] <- 0
drug_answers[drug_answers==2] <- 1
drug_answers[drug_answers==3] <- 3
drug_answers[drug_answers==4] <- 6
drug_answers[drug_answers==5] <- 10
drug_answers[drug_answers==6] <- 20
drug_answers[drug_answers==7] <- 40

recoded81012$minimum_experimentation <- rowSums(drug_answers)

recoded81012$D25[recoded81012$minimum_experimentation==0] <- 1 # None
recoded81012$D25[recoded81012$minimum_experimentation %in% c(1:2)] <- 2 # 1-2 days
recoded81012$D25[recoded81012$minimum_experimentation %in% c(3:5)] <- 3 # 3-5 days
recoded81012$D25[recoded81012$minimum_experimentation %in% c(6:9)] <- 4 # 6-9 days
recoded81012$D25[recoded81012$minimum_experimentation > 9] <- 5 # 10+ days

## recode 8-10-12 D2A
# D2A	On how many occasions (if any) have you used marijuana in your lifetime?
# D2B	Have you ever, even once in your lifetime, smoked marijuana?
recoded81012$D2B <- NA
recoded81012$D2B[recoded81012$D2A > 1] <- 1 # Yes
recoded81012$D2B[recoded81012$D2A == 1] <- 2 # No

## recode 8-10-12 D3A
# D3A	On how many occasions (if any) have you used marijuana during the past 30 days?
# D3B	During the past 30 days on how many days did you use marijuana?
recoded81012$D3B <- recoded81012$D3A
recoded81012$D3B[recoded81012$D3A > 4] <- 5

## recode 8-10-12 D8A
# D8A	On how many occasions (if any) have you sniffed glue, breathed (huffed) the contents of an aerosol spray can, or inhaled other gases or sprays in order to get high in your lifetime?
# D8B	Have you ever, even once in your lifetime, sniffed glue, breathed contents of aerosol spray can, or inhaled gases or sprays in order to get high?
recoded81012$D8B <- NA
recoded81012$D8B[recoded81012$D8A > 1] <- 1 # Yes
recoded81012$D8B[recoded81012$D8A == 1] <- 2 # No

## recode 8-10-12 D9A
# D9A	On how many occasions (if any) have you sniffed glue, breathed (huffed) the contents of an aerosol spray can, or inhaled other gases or sprays in order to get high in the past 30 days?
# D9B	During the past 30 days on how many days did you sniff glue, breathe (huff) the contents of an aerosol spray can, or inhale other gases or sprays in order to get high?
recoded81012$D9B <- recoded81012$D9A
recoded81012$D9B[recoded81012$D9A > 4] <- 5


## recode 8-10-12 H16,H17,H18,H19
# H16,H17,H18,H19 During the past 7 days, how many times did you eat green salad, potatoes, carrots, other?
# H6	During the past 7 days, how many times did you eat vegetables?
veggies_answers <- eighth2015[, c("H16","H17","H18","H19")]
View(data.frame(value = seq(1,7,1), min = c(0,1,4,1,2,3,4)))

veggies_answers[veggies_answers==1] <- 0
veggies_answers[veggies_answers==2] <- 2 # 2 times/day instead of 1-3 times/day
veggies_answers[veggies_answers==3] <- 5 # 5 times/day instead of 4-6 times/day
veggies_answers[veggies_answers==4] <- 7
veggies_answers[veggies_answers==5] <- 14
veggies_answers[veggies_answers==6] <- 21
veggies_answers[veggies_answers==7] <- 28

recoded81012$min_veggie_consumption <- rowSums(veggies_answers)
round(sort(unique(recoded81012$min_veggie_consumption))/7, 0)

View(data.frame(total = sort(unique(recoded81012$min_veggie_consumption)), rounded = round(sort(unique(recoded81012$min_veggie_consumption))/7, 0)))

recoded81012$H6[recoded81012$min_veggie_consumption == 0] <- 1 # No vegetables
recoded81012$H6[recoded81012$min_veggie_consumption %in% c(1:3)] <- 2 # 1-3 vegetables total
recoded81012$H6[recoded81012$min_veggie_consumption %in% c(4:6)] <- 3 # 4-6 vegetables total
recoded81012$H6[recoded81012$min_veggie_consumption %in% c(7:13)] <- 4 # 1 time per day
recoded81012$H6[recoded81012$min_veggie_consumption %in% c(14:17)] <- 5 # 2 times per day
recoded81012$H6[recoded81012$min_veggie_consumption %in% c(18:24)] <- 6 # 3 times per day
recoded81012$H6[recoded81012$min_veggie_consumption > 24] <- 7 # 4+ times per day

## recode 8-10-12 Q2A
# Q2A	On how many occasions (if any) have you used cabeniferol (cabbies) in your lifetime?
# Q2B	Have you ever, even once in your lifetime, used cabeniferol (“cabbies”)?
recoded81012$Q2B <- NA
recoded81012$Q2B[recoded81012$Q2A == 1] <- 2 # "No"
recoded81012$Q2B[recoded81012$Q2A > 1] <- 1 # "Yes" ... zero of these because it's a fake drug - anyone who answers "Yes" is removed.

## recode 8-10-12 Q3A
# Q3A	On how many occasions (if any) have you used cabeniferol (cabbies)  in the past 30 days?
# Q3B	During the past 30 days, on how many days did you use cabeniferol (“cabbies”)?
recoded81012$Q3B <- recoded81012$Q3A
recoded81012$Q3B[recoded81012$Q3A > 4] <- 5

# recode 8-10-12 T4A
# T4A	How often have you smoked cigarettes during the past 30 days?
# T4B	How often have you smoked cigarettes during the past 30 days?
recoded81012$T4B <- recoded81012$T4A
recoded81012$T4B[recoded81012$T4A > 5] <- 6


## check ----
length(intersect(names(sixth2015), names(recoded81012)))
dim(sixth2015)

# every 6th grade variable is now matched with an 8-10-12 variable!

## combine 8-10-12 and 6th grade surveys ----
merge81012 <- recoded81012[,names(recoded81012) %in% names(sixth2015)]
names(merge81012) %in% names(sixth2015)

# # test: rbind by column name works!
# row1 = data.frame(A = c('a', 'a'), B = c('b','b'))
# row2 = data.frame(B = c('b','b'), A = c('a', 'a'))
# rbind(row1, row2)

sixth2015$survey <- "6th Grade 2015"
merge81012$survey <- "8-10-12th Grade 2015"

fys2015_combined <- rbind(sixth2015, merge81012)

# check
dim(fys2015_combined)

dim(sixth2015)
dim(merge81012)
