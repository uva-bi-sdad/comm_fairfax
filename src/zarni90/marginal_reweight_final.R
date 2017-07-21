# get marginal ACS distributions (multinomials)
# reweight imputed samples by marginal distribution
# return the result; 100 synthetic populations
# analyze a) marginal distributions, b) conditional distributions for one result

# ------------------------------------------------------------
# load in, analyze the imputed samples
#PUMS CHANGE IN MARGINAL DISTRIBUTION


#LEFT TO DO:
#Uniform distribution attachment:


#ZCTAs slightly beyond  high school boundaries



#2% doesn't have
load("miceoutput.Rdata")
library(mice)

# 7 categorical variables. All are changed to factors
# columns: RAC1P, SEX, AGEP, DREM, PINCP, PAP, ENG

# RAC1P: 1-9
# SEX: 1-2
# ENG: 1-5
# DREM: 1-3 (Zarni coded 3 as 'under 5'. Though ~4% of imputed ended up over 5.
# Not tht importnt.
# 1=yes, 2=no
#   Better to do 1-2 and just set under 5s to NA after?)
# PINCP: 1-8 categories: Lower to higher income based on the ACS income
# PAP : 1 = Do not receive public asssitance, 2 = Receive public assistances
# AGEP: 1 to 4: In chunks of 25 years old

pums_all_fairfax <- rio::import("~/git/comm_fairfax/data/comm_fairfax/working/pums_all_fairfax.csv")[-1]
names(pums_all_fairfax)



# get the first imputed sample
#test <- complete(mice.out,1)


sum(test$AGEP < 0) / nrow(test)
sum(test$AGEP > 100) / nrow(test)

#Bi-modal
#pums cut it off at 2000
#If our response looks normal. I will log transform it.
#Transform it and cap it at a certain value


# PAP: continuous, this one is really weird
# more than half are negative, 7% are zero
# need to look at distribution in PUMS; remove negative values and outliers, do a transformation


sort(table(test$PAP),decreasing=T)[1:10]
sum(test$PAP >= 0) / nrow(test)
sum(test$PAP == 0) / nrow(test)

# PINCP:
# -20,000 to 1 million, highly right skewed
# 25% are negative; see if this gets smaller under a good transformation (e.g. log)
hist(test$PINCP)
sum(test$PINCP < 0) / nrow(test)


# ------------------------------------------------------------
# read in marginal tables by ZCTA

# Tables:
# B06010: Place of birth by individual income in the past 12 months (in 2015 adjusted dollars in the U.S)
# B18104: SEX by AGE by Cognitive Difficulty
# B16001: Langugage Spoken at Home by ability to speak English for the Population 5 years and over
# B19057: Public Assistance Income in the past 12 months for Households
# C02003: Detailed Race Universe: Total Population
# B01001: Sex By age
# B01001: Sex By age

library(acs)
library(maptools)
library(dplyr)
library(tigris)
library(sp)

#load in API key
api.key.install("a6f5f5a822ad65a230b64f035337bb393b404bb7")

#to call in ACS tables
zip_matcher <- function(tablecode, ziplist){
    #Get everything in zip code
    FX.VA<-geo.make(zip.code = "*")
    #Fetch the particular table
    FX.VA.TP<-acs.fetch(geography=FX.VA, endyear=2015, table.number=tablecode, col.name="pretty")
    #May need to modify depending on table
    FXCStats<-data.frame(estimate(FX.VA.TP), round(standard.error(FX.VA.TP),0))
    #I have deleted out the colnames. We can put them up when we need to.
    #colnames(FXCStats)<-c("Total","SE.Total")
    #Establishing Zipcode from Rownames
    FXCStats$zipcode <- row.names(FXCStats)
    FXCStats$zipcode
    #Spliting the String part and the numeric part of Zipcode
    zip_split <- str_split(FXCStats$zipcode, pattern = " ", n = 2)
    zipcode <- sapply(zip_split, function(x) x[2])
    FXCStats$zipcode <- zipcode
    #Importing the Master ZipCode File
    ziplist <- as.data.frame(ziplist)
    colnames(ziplist) <- "zipcode"
    ziplist$zipcode <- as.character(ziplist$zipcode)
    FXCStats$zipcode <- as.character(FXCStats$zipcode)
    #Joining the two tables by ZipCode
    FX_Zip_Final <- ziplist %>% left_join(FXCStats, by = "zipcode")
    return (FX_Zip_Final)
}

# get Fairfax county ZCTAs, store in 'ziplist'
ffx_person <- read.csv("~/git/comm_fairfax/data/comm_fairfax/working/pums_all_fairfax.csv")[,-1]
zipdat <- as.data.frame(table(ffx_person$ZCTAS))
names(zipdat) <- c("zip","n")

# ------------------------------------------------------------

acs_RAC1P <- zip_matcher("C02003",zipdat$zip)
View(acs_RAC1P)
ncol(acs_RAC1P)
# 1 .White alone
# 2 .Black or African American alone
# 3 .American Indian alone
# 4 .Alaska Native alone
# 5 .American Indian and Alaska Native tribes specified; or American
# .Indian or Alaska Native, not specified and no other races
# 6 .Asian alone
# 7 .Native Hawaiian and Other Pacific Islander alone
# 8 .Some Other Race alone
# 9 .Two or More Races
acs_RAC1P2 <- data.frame(zipcode=acs_RAC1P$zipcode, 'white'=acs_RAC1P[,4],
                         'black'=acs_RAC1P[,5],
                         #'cat3_4_5_7'=rowSums(acs_RAC1P[,c(6,8)]),
                         'asian'=acs_RAC1P[,7],
                         #'pacific_islander'=acs_RAC1P[,8],
                         'other'=rowSums(acs_RAC1P[,c(6,8,9)]),
                         'two_or_more'=acs_RAC1P[,10])
# problem: there still a few zeroes
# do some cheap 'zero inflation'
View(acs_RAC1P2)

acs_RAC1P3 <- acs_RAC1P2
#WARNING HERE!
acs_RAC1P3[ acs_RAC1P3 < 100 ] <- 100
acs_RAC1P_prob <- acs_RAC1P3[2:ncol(acs_RAC1P3)]/rowSums(acs_RAC1P3[2:ncol(acs_RAC1P3)])
acs_RAC1P_prob <- cbind(zipcode=acs_RAC1P$zipcode, acs_RAC1P_prob)



acs_SEX <- zip_matcher("B01001",zipdat$zip)
# 1=male, 2=female
acs_SEX2 <- data.frame(zipcode=acs_SEX$zipcode, male=acs_SEX$Sex.by.Age..Male.,
                       female=acs_SEX$Sex.by.Age..Female.)
acs_SEX_prob <- acs_SEX2[2:ncol(acs_SEX2)]/rowSums(acs_SEX2[2:ncol(acs_SEX2)])
acs_SEX_prob <- cbind(zipcode=acs_SEX$zipcode, acs_SEX_prob)
View(acs_SEX_prob)


acs_AGEP2 <- data.frame(zipcode=acs_SEX$zipcode,
                        age0_25=rowSums( acs_SEX[,c(4:11,28:35)]),
                        age25_50=rowSums( acs_SEX[,c(12:16,36:40)] ),
                        age50_75=rowSums( acs_SEX[,c(17:23,41:47)] ),
                        age75_up=rowSums( acs_SEX[,c(24:26,48:50)] ) )
acs_AGEP_prob <- acs_AGEP2[2:ncol(acs_AGEP2)]/rowSums(acs_AGEP2[2:ncol(acs_AGEP2)])
acs_AGEP_prob <- cbind(zipcode=acs_AGEP2$zipcode, acs_AGEP_prob)
# break age into categories: 0-25, 25-50, 50-75, 75+
acs_AGE_breaks <- c(-Inf,25,50,75,Inf)


acs_DREM <- zip_matcher("B18104",zipdat$zip)
View(acs_DREM)

# 1=yes, 2=no, 3=under 5
acs_DREM2 <- data.frame(zipcode=acs_DREM$zipcode,
                        cognitive_difficulty = rowSums( acs_DREM[,c(5,8,11,14,17,21,24,27,30,33)] ),
                        no_cognitive_diffiulty = rowSums( acs_DREM[,c(6,9,12,15,18,22,25,28,31,34)] ),
                        under_5 = rowSums( acs_SEX[,c(4,28)]) )
acs_DREM_prob <- acs_DREM2[2:ncol(acs_DREM2)]/rowSums(acs_DREM2[2:ncol(acs_DREM2)])
acs_DREM_prob <- cbind(zipcode=acs_DREM$zipcode, acs_DREM_prob)

# match on these four for now; hold off on ENG, PAP, PINCP

#ENGLISH! #DROP THIS FOR NOW
#Questions for Josh: regarding categorizing them
acs_ENG1 <- zip_matcher("B16001", zipdat$zip)
acs_ENG1
#very well indexes : 3, 5, 8, 11, ... 238
#less than well indexes : 6, 9, 12,... 239
#QUERY on how to fix the indexes here!
acs_ENG2 <- data.frame(zipcode = acs_ENG1$zipcode,
                       very_well = acs_ENG1[,c()],
                       not_well = acs_ENG1[,c()])
acs_ENG2_prob <- acs_ENG2[2:ncol(acs_ENG2)]/rowSums(acs_ENG2[2:ncol(acs_ENG2)])


View(acs_ENG1)
names(acs_ENG1)

#PAP
acs_PAP1 <- zip_matcher("B19057", zipdat$zip)
View(acs_PAP1)
#Should I include an option for under 5?
acs_PAP2 <- data.frame(zipcode = acs_DREM$zipcode,
                       public_assistance = acs_PAP1[,c(3)],
                       nopublic_assistance = acs_PAP1[,c(4)])
acs_PAP2_prob <- acs_PAP2[2:ncol(acs_PAP2)]/rowSums(acs_PAP2[2:ncol(acs_PAP2)])

#PINCP
acs_PINCP1 <- zip_matcher("B06010", zipdat$zip)
View(acs_PINCP1)
acs_PINCP2 <- data.frame(zipcode = acs_PINCP1$zipcode,
                         income_1 = rowSums(acs_PINCP1[,c(3,5)]), #0 and 1-9999 #Row Sums
                         income_2 = acs_PINCP1[,c(6)],
                         income_3 = acs_PINCP1[,c(7)],
                         income_4 = acs_PINCP1[,c(8)],
                         income_5 = acs_PINCP1[,c(9)],
                         income_6 = acs_PINCP1[,c(10)],
                         income_7 = acs_PINCP1[,c(11)],
                         income_8 = acs_PINCP1[,c(12)])
acs_PINCP2_prob <- acs_PINCP2[2:ncol(acs_PINCP2)]/rowSums(acs_PINCP2[2:ncol(acs_PINCP2)])
#If the bin size is too
#Group








View(acs_PINCP2_prob)

# -----------------------------------------------------------------------
# draw imputed samples from the marginal distribution (independantly for each person)
# marginal weights are the product of marginal probabilities for each variable
# -----------------------------------------------------------------------


#change the row number here!
imp_draws <- list()
for(i in 1:5) {
    imp_draws[[i]] <- cbind(zipcode=ffx_person$ZCTAS,complete(mice.out,i)[1:nrow(ffx_person),])
}

marginal_weights <- matrix(NA,nrow=nrow(imp_draws[[1]]),ncol=length(imp_draws))
marginal_samp_prob <- matrix(NA,nrow=nrow(imp_draws[[1]]),ncol=length(imp_draws))

acs_marginal <- function(zip_codes, values, acs_prob, value_to_column) {
    rownum <- match(zip_codes,acs_prob$zipcode)
    colnum <- value_to_column[values]
    weights <- rep(NA,length(rownum))
    for(i in 1:length(rownum)){ weights[i] <- acs_prob[rownum[i],colnum[i]] }
    return(weights)
}


for(i in 1:length(imp_draws)){
    # bin age according to breaks
    imp_draws[[i]]$AGE_F <- as.numeric( cut(imp_draws[[i]]$AGEP,breaks=acs_AGE_breaks) )
    # weights are a product of the marginal distributions
    marginal_weights[,i] <-
        acs_marginal(imp_draws[[i]]$zipcode,imp_draws[[i]]$RAC1P,
                     acs_RAC1P_prob,value_to_column <- c(2,3,5,5,5,4,5,5,6)) *
        acs_marginal(imp_draws[[i]]$zipcode,imp_draws[[i]]$DREM,
                     acs_DREM_prob,value_to_column <- 2:4) *
        acs_marginal(imp_draws[[i]]$zipcode,imp_draws[[i]]$SEX,
                     acs_SEX_prob,value_to_column <- 2:3) *
        acs_marginal(imp_draws[[i]]$zipcode,imp_draws[[i]]$AGE_F,
                     acs_AGEP_prob,value_to_column <- 2:5)
    marginal_samp_prob[,i] <- marginal_weights[,i]/sum(marginal_weights[,i])
    if(i%%5==0){print(i)}
}

# are the sampling probabilities stable?
# what are we checking for stability here?

sort(marginal_samp_prob[,1],decreasing = T)[1:100]
sort(marginal_samp_prob[,1],decreasing = F)[1:100]
max(marginal_samp_prob[,1])/min(marginal_samp_prob[,1])
# hist(log(marginal_samp_prob[,1]))

# draw samples from the weighted marginal distribution
# (samples are a list of data frames)
ndraws <- 1

#This part I might have to ask you again!
#For the redistricting map, do you draw it from one final imputed data set.

imp_draws_weighted <- list()
for(i in 1:ndraws){
    imp_draws_weighted[[i]] <- imp_draws[[1]]
}
# take each *row* from the sampled draw
for(i in 1:nrow(imp_draws_weighted[[i]])){
    ind_draw <- sample(size=ndraws, x=1:numdraws, prob=marginal_samp_prob[i,], replace=TRUE)
    for(j in 1:ndraws){
        imp_draws_weighted[[i]][[j]][i,] <- imp_draws[[ind_draw[j]]][i,]
    }
}

save.image("mice_reweighted.RData")

# -----------------------------------------------------------------------
# Analyze reweighted samples
# Check conditional distributions
# Compare marginals by zip code
# -----------------------------------------------------------------------

