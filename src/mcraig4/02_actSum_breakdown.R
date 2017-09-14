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
