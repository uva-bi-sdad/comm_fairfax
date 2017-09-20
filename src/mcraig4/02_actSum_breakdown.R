# This file is the summary file from the ATUS data with each respondent's state included, as well as a grouping variable for their age. Note that I've removed hawaii and alaska since the maping software I'm using doesn't have shapefiles for them, not for any good reason.

actSum = read.csv("./data/comm_fairfax/working/ATUS_data_working/ATUSsummaryAgeState.csv")


personal_care <- actSum[, (28:39)]
household_act <- actSum[, (40:71)]
helping_HH_mem <- actSum[, (72:104)]
helping_nHH_mem <- actSum[, (105:141)]
work_act <- actSum[, (142:161)]
education <- actSum[, (162:179)]
consum_purc <- actSum[, (180:190)]
prof_pers_care <- actSum[, (191:216)]
house_serv <- actSum[, (217:234)]
civic_obli <- actSum[, (235:246)]
eat_drink <- actSum[, (247:251)]
social_relax <- actSum[, (252:282)]
sports_rec <- actSum[, (283:359)]
religious <- actSum[, (360:365)]
volunteer <- actSum[, (366:389)]
telephone <- actSum[, (390:398)]
travel <- actSum[, (399:451)]


one <- data.frame(colSums(personal_care, dims = 1))
two <- data.frame(colSums(household_act, dims = 1))
three <- data.frame(colSums(helping_HH_mem, dims = 1))
four <- data.frame(colSums(helping_nHH_mem, dims = 1))
five <- data.frame(colSums(work_act, dims = 1))
six <- data.frame(colSums(education, dims = 1))
seven <- data.frame(colSums(consum_purc, dims = 1))
eight <- data.frame(colSums(prof_pers_care, dims = 1))
nine <- data.frame(colSums(house_serv, dims = 1))
ten <- data.frame(colSums(civic_obli, dims = 1))
eleven <- data.frame(colSums(eat_drink, dims = 1))
twelve <- data.frame(colSums(social_relax, dims = 1))
thirteen <- data.frame(colSums(sports_rec, dims = 1))
fourteen <- data.frame(colSums(religious, dims = 1))
fifteen <- data.frame(colSums(volunteer, dims = 1))
sixteen <- data.frame(colSums(telephone, dims = 1))
eighteen <- data.frame(colSums(travel, dims = 1))

# This will do a cross tabulation for their major categories by state and by age group.

personal_care_state_age = data.frame(tapply(rowSums(personal_care), list(actSum$state, actSum$agegrp), mean))
colMeans(personal_care_state_age)

household_act_state_age = data.frame(tapply(rowSums(household_act), list(actSum$state, actSum$agegrp), mean))
colMeans(household_act_state_age)

work_act_state_age <- data.frame(tapply(rowSums(work_act), list(actSum$state, actSum$agegrp), mean))
colMeans(work_act_state_age)

social_relax_state_age <- data.frame(tapply(rowSums(social_relax), list(actSum$state, actSum$agegrp), mean))
colMeans(social_relax_state_age)

eat_drink_state_age <- data.frame(tapply(rowSums(eat_drink), list(actSum$state, actSum$agegrp), mean))
colMeans(eat_drink_state_age)


work0501 <- data.frame(work_act[, 1])
work_act_state_age <- data.frame(tapply(rowSums(work0501), list(actSum$state, actSum$agegrp), mean))
colMeans(work_act_state_age)


sleep101 <- data.frame(personal_care[, 1])
sleep101_state_age <- data.frame(tapply(rowSums(sleep101), list(actSum$state, actSum$agegrp), mean))
colMeans(sleep101_state_age)
