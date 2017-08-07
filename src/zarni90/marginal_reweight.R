# get marginal ACS distributions (multinomials)
# reweight imputed samples by marginal distribution
# return the result; 100 synthetic populations
# analyze a) marginal distributions, b) conditional distributions for one result

# ------------------------------------------------------------
# load in, analyze the imputed samples
#PUMS CHANGE IN MARGINAL DISTRIBUTION

#ZCTAs slightly beyond  high school boundaries
#uniform address:

#2% doesn't have
load("~/git/comm_fairfax/data/comm_fairfax/working/miceoutput.Rdata")
library(mice)

# 4 categorical, 3 continuous variables
# columns: RAC1P, SEX, AGEP, DREM, PINCP, PAP, ENG

# get the first imputed sample
test <- mice::complete(mice.out,1)

# RAC1P: 1-9
# SEX: 1-2
# ENG: 1-5

# DREM: 1-3 (Zarni coded 3 as 'under 5'. Though ~4% of imputed ended up over 5.
# Not tht importnt.
# 1=yes, 2=no
#   Better to do 1-2 and just set under 5s to NA after?)

# AGEP: continuous, imputed is ~ -50-150 (not too bad, but PUMS dist is bimodal; PMM?)
# only 5% are negative; set them to 0 for now
# .3% are > 100; set to 100
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
# S1901: Income in the past 12 months
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


#MODIFY THE ZIP MATCHERS HERE TO RUN ONLY FAIRFAX!
#to call in tables
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
#ffx_person <- read.csv("~/mounts/lightfoot/sdal/projects/comm_fairfax/working/pums_all_fairfax.csv")[,-1]
ffx_person <- read.csv("~/git/comm_fairfax/data/comm_fairfax/working/pums_all_fairfax.csv")[,-1]

zipdat <- as.data.frame(table(ffx_person$ZCTAS))
head(zipdat)
names(zipdat) <- c("zip","n")

# ------------------------------------------------------------

acs_RAC1P <- zip_matcher("C02003",zipdat$zip)
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

View(acs_RAC1P)
dim(acs_RAC1P)

#Check ACS Table breakdowns here to understand what Josh is doing.
acs_RAC1P2 <- data.frame(zipcode=acs_RAC1P$zipcode, 'white'=acs_RAC1P[,4],
                         'black'=acs_RAC1P[,5],
                         #'cat3_4_5_7'=rowSums(acs_RAC1P[,c(6,8)]),
                         'asian'=acs_RAC1P[,7],
                         #'pacific_islander'=acs_RAC1P[,8],
                         'other'=rowSums(acs_RAC1P[,c(6,8,9)]),
                         'two_or_more'=acs_RAC1P[,10])

head(acs_RAC1P2)
# problem: there still a few zeroes
# do some cheap 'zero inflation'
#This is where some people do not exist in some ZCTAs. So, we have to impute people into those ZCTAs assuming, we are only drawing from a sample.
acs_RAC1P3 <- acs_RAC1P2
nrow(acs_RAC1P3)
acs_RAC1P3$white[acs_RAC1P3$white < 100] <- 100
acs_RAC1P3$black[acs_RAC1P3$black < 100] <- 100
acs_RAC1P3$asian[acs_RAC1P3$asian < 100] <- 100
acs_RAC1P3$other[acs_RAC1P3$other < 100] <- 100
acs_RAC1P3$two_or_more[acs_RAC1P3$two_or_more < 100] <- 100

acs_RAC1P_prob <- acs_RAC1P3[2:ncol(acs_RAC1P3)]/rowSums(acs_RAC1P3[2:ncol(acs_RAC1P3)]) #We calculate the marginal probabilities for a person from that ACTA
acs_RAC1P_prob <- cbind(zipcode=acs_RAC1P$zipcode, acs_RAC1P_prob) #Recombine this back with Zipcodes

View(acs_RAC1P_prob)

#SEX
acs_SEX <- zip_matcher("B01001",zipdat$zip)
# 1=male, 2=female
acs_SEX2 <- data.frame(zipcode=acs_SEX$zipcode, male=acs_SEX$Sex.by.Age..Male.,
                       female=acs_SEX$Sex.by.Age..Female.)
acs_SEX_prob <- acs_SEX2[2:ncol(acs_SEX2)]/rowSums(acs_SEX2[2:ncol(acs_SEX2)])
acs_SEX_prob <- cbind(zipcode=acs_SEX$zipcode, acs_SEX_prob)


#What is under acs_sex?
#It's going to be the same table code
View(acs_SEX)


acs_AGEP2 <- data.frame(zipcode=acs_SEX$zipcode,
                        age0_25=rowSums( acs_SEX[,c(4:11,28:35)]),
                        age25_50=rowSums( acs_SEX[,c(12:16,36:40)] ),
                        age50_75=rowSums( acs_SEX[,c(17:23,41:47)] ),
                        age75_up=rowSums( acs_SEX[,c(24:26,48:50)] ) )
acs_AGEP_prob <- acs_AGEP2[2:ncol(acs_AGEP2)]/rowSums(acs_AGEP2[2:ncol(acs_AGEP2)])
acs_AGEP_prob <- cbind(zipcode=acs_AGEP2$zipcode, acs_AGEP_prob)
# break age into categories: 0-25, 25-50, 50-75, 75+

#WHY -INF, here?
acs_AGE_breaks <- c(-Inf,25,50,75,Inf)


acs_DREM <- zip_matcher("B18104",zipdat$zip)
# 1=yes, 2=no, 3=under 5
acs_DREM2 <- data.frame(zipcode=acs_DREM$zipcode,
                        cognitive_difficulty = rowSums( acs_DREM[,c(5,8,11,14,17,21,24,27,30,33)] ),
                        no_cognitive_diffiulty = rowSums( acs_DREM[,c(6,9,12,15,18,22,25,28,31,34)] ),
                        under_5 = rowSums( acs_SEX[,c(4,28)]) )
acs_DREM_prob <- acs_DREM2[2:ncol(acs_DREM2)]/rowSums(acs_DREM2[2:ncol(acs_DREM2)])
acs_DREM_prob <- cbind(zipcode=acs_DREM$zipcode, acs_DREM_prob)

# match on these four for now; hold off on ENG, PAP, PINCP

#for ENG
acs_ENG <- zip_matcher("B16001", zipdat$zip)
View(acs_ENG)
#English: Speaks very well AND Speaks English Less than Well # ACS: 5 years and over
#English: Lessthan 5 years old/speaks only English (Very well, well, Not well, not at all)

#for PAP
acs_PAP <- zip_matcher("B19057",zipdat$zip)
View(acs_PAP)

#for PINCP # Has to be manually downloaded: It only has 2015- 5 year estimates.

acs_PINCP <- rio::import("~/git/comm_fairfax/data/comm_fairfax/working/ACS_15_5YR_S1901_with_ann.csv")
View(acs_PINCP)

# -----------------------------------------------------------------------
# draw imputed samples from the marginal distribution (independantly for each person)
# marginal weights are the product of marginal probabilities for each variable
# -----------------------------------------------------------------------


#Need to fix the row subset from here. It's not 1:nrow()
imp_draws <- list()
test <- complete(mice.out, 1)
#Getting all the imputations into a list of data frames
for(i in 1:5) {
    imp_draws[[i]] <- cbind(zipcode=ffx_person$ZCTAS,complete(mice.out,i)[83473:nrow(test),])
}

#Marginal weights :: #Getting the Marginal Weights data frame
marginal_weights <- matrix(NA,nrow=nrow(imp_draws[[1]]),ncol=length(imp_draws)) # 5 imputed draws
#marginal_samp_prob <- matrix(NA,nrow=nrow(imp_draws[[1]]),ncol=length(imp_draws)) #ACS

#UNDERSTAND THIS LATER: PRETTY CLOSE
#This is the Key Function:
acs_marginal <- function(zip_codes, values, acs_prob, value_to_column) {
    rownum <- match(zip_codes,acs_prob$zipcode) #Matching rows by zipcode
    #Values are imputed draws
    #browser()
    colnum <- value_to_column[values] #matching PUMS factors to ACS probability factors that you have created!
    weights <- rep(NA,length(rownum)) #However many rows we have: however many zip codes we have (Re-weight)
    for(i in 1:length(rownum)){ weights[i] <- acs_prob[rownum[i],colnum[i]] } #What is acs_prob doing? Where are you summing here? #acs probability matrix 2:6: Only 1 value: The person is White: Vectorize it
                                                    #rownum is the zip code this person lists in i is the zip code column is the variable of interest
                                                    #acs table has the probability from earlier
                                                    #It's pinpoining that within a particular zipcode, if a person is black, the probability is 0.05672
    return(weights) #For each row and column we have a weight
}

c(2, 3,3)[values] #2 corresponds to 2nd value of race in ACS: columns are being given imputed draws

test <- acs_marginal(imp_draws[[1]]$zipcode,imp_draws[[1]]$RAC1P,
             acs_RAC1P_prob,value_to_column <- c(2,3,5,5,5,4,5,5,6))


for(i in 1:length(imp_draws)){
    # bin age according to breaks
    imp_draws[[i]]$AGE_F <- as.numeric( cut(imp_draws[[i]]$AGEP,breaks=acs_AGE_breaks) )
    # weights are a product of the joint marginal distributions of all the variables
    # Assuming marginal disributions are independent
    #What is the probability we observe a person with the given attributes from that zip code.
    #Since we have five, we will pick the one with the highest probability: We will sample according to
    #thier probability
    marginal_weights[,i] <-
        acs_marginal(imp_draws[[i]]$zipcode,imp_draws[[i]]$RAC1P,
                     acs_RAC1P_prob,value_to_column <- c(2,3,5,5,5,4,5,5,6)) * # Compare PUMS to ACS above: As they are factor variables 1 in PUMS (White) is 2, 2 Black is 3 Black in ACS etc. It's rematching!
        acs_marginal(imp_draws[[i]]$zipcode,imp_draws[[i]]$DREM,
                     acs_DREM_prob,value_to_column <- 2:4) *
        acs_marginal(imp_draws[[i]]$zipcode,imp_draws[[i]]$SEX,
                     acs_SEX_prob,value_to_column <- 2:3) *
        acs_marginal(imp_draws[[i]]$zipcode,imp_draws[[i]]$AGE_F,
                     acs_AGEP_prob,value_to_column <- 2:5)
    #marginal_samp_prob[,i] <- marginal_weights[,i]/sum(marginal_weights[,i])
    #If we do it for 5 imputed data set, we have the 5 values the marginal probability of seeing a person
    #with these characteristics. We have a distribution of marginal probabilities of a person having all these attributes.
    #Which one do we pick as the topmost?

    if(i%%5==0){print(i)}
}

# are the sampling probabilities stable?
sort(marginal_weights[,1],decreasing = T)[1:100]
sort(marginal_weights[,1],decreasing = F)[1:100]
max(marginal_weights[,1])/min(marginal_samp_prob[,1]) #What is this?
# hist(log(marginal_samp_prob[,1]))

# draw samples from the weighted marginal distribution
# (samples are a list of data frames)
ndraws <- 2

imp_draws_weighted <- list()
for(i in 1:ndraws){
    imp_draws_weighted[[i]] <- imp_draws[[i]]
}
# take each *row* from the sampled draw
#We would want a distribution of values for the person
#Lots of checks
#If we want an error in the method, we can compute sample error
#Average estimtae by high school error: We want average percent:
#Get errors in the final estimate
#From each of the 5 imputed data set: for each row of ZCTA:
#we get the marginal probablity of that person being
#in that ZCTA: so there are 5 marginal probabilities:
#
#Fixed numdraws to ndraws
#Fixed marginal_weights[i,1:ndraws]
#FIX THE CODE HERE!
for(i in 1:nrow(imp_draws_weighted[[i]])){
    ind_draw <- sample(size=ndraws, x=1:ndraws, prob=marginal_weights[i,1:ndraws], replace=TRUE)
    browser()
    for(j in 1:ndraws){
        browser()
        imp_draws_weighted[[i]][[j]][i,] <- imp_draws[[ind_draw[j]]][i,]
        #Given the marginal weights, it's picking up from which imputed data set we are going to
        #use to replace the value:
        #So if the marginal weights so 30% is white, 30 year old cognitive difficulty etc: We will
        #see that 30% of the time in our 100 imputed draw
        browser()
    }
}

save.image("mice_reweighted.RData")

# -----------------------------------------------------------------------
# Analyze reweighted samples
# Check conditional distributions
# Compare marginals by zip code
# -----------------------------------------------------------------------

