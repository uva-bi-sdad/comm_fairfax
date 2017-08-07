f# do 'easy' imputation
library(dplyr)

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
library(mice)

#load in API key
api.key.install("a6f5f5a822ad65a230b64f035337bb393b404bb7")


#Loading the imputed data set
load("~/git/comm_fairfax/data/comm_fairfax/final/miceoutput_factor.Rdata")
fairfax_all_latlong <- rio::import("~/git/comm_fairfax/data/comm_fairfax/final/fairfax_latlong.csv")
pums_person_interest <- rio::import("~/git/comm_fairfax/data/comm_fairfax/working/pums_person_interest_factors.csv")

#Add loading pums person interest

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
zipdat <- as.data.frame(table(fairfax_all_latlong$ZCTAS))
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
acs_RAC1P2 <- data.frame(zipcode=acs_RAC1P$zipcode, 'white'=acs_RAC1P[,4],
                         'black'=acs_RAC1P[,5],
                         #'cat3_4_5_7'=rowSums(acs_RAC1P[,c(6,8)]),
                         'asian'=acs_RAC1P[,7],
                         #'pacific_islander'=acs_RAC1P[,8],
                         'other'=rowSums(acs_RAC1P[,c(6,8,9)]),
                         'two_or_more'=acs_RAC1P[,10])
# problem: there still a few zeroes
# do some cheap 'zero inflation'
acs_RAC1P2$white[ acs_RAC1P2$white < 100 ] <- 100
acs_RAC1P2$black[ acs_RAC1P2$black < 100 ] <- 100
acs_RAC1P2$asian[ acs_RAC1P2$asian < 100 ] <- 100
acs_RAC1P2$other[ acs_RAC1P2$other < 100 ] <- 100
acs_RAC1P2$two_or_more[ acs_RAC1P2$two_or_more < 100 ] <- 100

acs_RAC1P3 <- acs_RAC1P2
acs_RAC1P_prob <- acs_RAC1P3[2:ncol(acs_RAC1P3)]/rowSums(acs_RAC1P3[2:ncol(acs_RAC1P3)])
acs_RAC1P_prob <- cbind(zipcode=acs_RAC1P$zipcode, acs_RAC1P_prob)
# clean up
rm(acs_RAC1P2,acs_RAC1P3)


acs_SEX <- zip_matcher("B01001",zipdat$zip)
# 1=male, 2=female
acs_SEX2 <- data.frame(zipcode=acs_SEX$zipcode, male=acs_SEX$Sex.by.Age..Male.,
                       female=acs_SEX$Sex.by.Age..Female.)
acs_SEX2$male[acs_SEX2$male < 100] <- 100
acs_SEX2$male[acs_SEX2$female < 100] <- 100
acs_SEX_prob <- acs_SEX2[2:ncol(acs_SEX2)]/rowSums(acs_SEX2[2:ncol(acs_SEX2)])
acs_SEX_prob <- cbind(zipcode=acs_SEX$zipcode, acs_SEX_prob)
# clean up
rm(acs_SEX2)


#Same table as sex
acs_AGEP2 <- data.frame(zipcode=acs_SEX$zipcode,
                        age0_25=rowSums( acs_SEX[,c(4:11,28:35)]),
                        age25_50=rowSums( acs_SEX[,c(12:16,36:40)] ),
                        age50_75=rowSums( acs_SEX[,c(17:23,41:47)] ),
                        age75_up=rowSums( acs_SEX[,c(24:26,48:50)] ) )
acs_AGEP2$age0_25[acs_AGEP2$age0_25 < 100] <- 100
acs_AGEP2$age25_50[acs_AGEP2$age25_50 < 100] <- 100
acs_AGEP2$age50_75[acs_AGEP2$age50_75 < 100] <- 100
acs_AGEP2$age75_up[acs_AGEP2$age75_up < 100] <- 100

acs_AGEP_prob <- acs_AGEP2[2:ncol(acs_AGEP2)]/rowSums(acs_AGEP2[2:ncol(acs_AGEP2)])
acs_AGEP_prob <- cbind(zipcode=acs_AGEP2$zipcode, acs_AGEP_prob)

# break age into categories: 0-25, 25-50, 50-75, 75+
acs_AGE_breaks <- c(-Inf,25,50,75,Inf)
# clean up
rm(acs_AGEP2)


acs_DREM <- zip_matcher("B18104",zipdat$zip)
# 1=yes, 2=no, 3=under 5
acs_DREM2 <- data.frame(zipcode=acs_DREM$zipcode,
                        cognitive_difficulty = rowSums( acs_DREM[,c(5,8,11,14,17,21,24,27,30,33)] ),
                        no_cognitive_diffiulty = rowSums( acs_DREM[,c(6,9,12,15,18,22,25,28,31,34)] ),
                        under_5 = rowSums( acs_SEX[,c(4,28)]) )
acs_DREM2$cognitive_difficulty[acs_DREM2$cognitive_difficulty < 100] <- 100
acs_DREM2$no_cognitive_diffiulty[acs_DREM2$no_cognitive_diffiulty < 100] <- 100
acs_DREM2$under_5[acs_DREM2$under_5 < 100] <- 100

acs_DREM_prob <- acs_DREM2[2:ncol(acs_DREM2)]/rowSums(acs_DREM2[2:ncol(acs_DREM2)])
acs_DREM_prob <- cbind(zipcode=acs_DREM$zipcode, acs_DREM_prob)
#View(acs_DREM_prob)
# clean up
rm(acs_DREM2)

#ENGLISH! #DROP THIS FOR NOW
#This is too tedious to match it back to the ACS table
#PAP

acs_PAP1 <- zip_matcher("B19057", zipdat$zip)
#Should I include an option for under 5?
acs_PAP2 <- data.frame(zipcode = acs_DREM$zipcode,
                       public_assistance = acs_PAP1[,c(3)],
                       nopublic_assistance = acs_PAP1[,c(4)])
acs_PAP2$public_assistance[acs_PAP2$public_assistance < 100] <- 100
acs_PAP2$nopublic_assistance[acs_PAP2$nopublic_assistance < 100] <- 100


acs_PAP2_prob <- acs_PAP2[2:ncol(acs_PAP2)]/rowSums(acs_PAP2[2:ncol(acs_PAP2)])
acs_PAP2_prob <- cbind(zipcode = acs_PAP2$zipcode, acs_PAP2_prob)
#clean up
rm(acs_PAP2)

#PINCP
acs_PINCP1 <- zip_matcher("B06010", zipdat$zip)
#View(acs_PINCP1)
acs_PINCP2 <- data.frame(zipcode = acs_PINCP1$zipcode,
                         income_1 = rowSums(acs_PINCP1[,c(3,5)]), #0 and 1-9999 #Row Sums
                         income_2 = acs_PINCP1[,c(6)],
                         income_3 = acs_PINCP1[,c(7)],
                         income_4 = acs_PINCP1[,c(8)],
                         income_5 = acs_PINCP1[,c(9)],
                         income_6 = acs_PINCP1[,c(10)],
                         income_7 = acs_PINCP1[,c(11)],
                         income_8 = acs_PINCP1[,c(12)])

acs_PINCP2$income_1[acs_PINCP2$income_1 < 100] <- 100
acs_PINCP2$income_2[acs_PINCP2$income_2 < 100] <- 100
acs_PINCP2$income_3[acs_PINCP2$income_3 < 100] <- 100
acs_PINCP2$income_4[acs_PINCP2$income_4 < 100] <- 100
acs_PINCP2$income_5[acs_PINCP2$income_5 < 100] <- 100
acs_PINCP2$income_6[acs_PINCP2$income_6 < 100] <- 100
acs_PINCP2$income_7[acs_PINCP2$income_7 < 100] <- 100
acs_PINCP2$income_8[acs_PINCP2$income_8 < 100] <- 100

acs_PINCP2_prob <- acs_PINCP2[2:ncol(acs_PINCP2)]/rowSums(acs_PINCP2[2:ncol(acs_PINCP2)])
acs_PINCP2_prob <- cbind(zipcode = acs_PINCP2$zipcode, acs_PINCP2_prob)
#clean up
rm(acs_PINCP2)

# -----------------------------------------------------------------------
# draw imputed samples from the marginal distribution (independantly for each person)
# marginal weights are the product of marginal probabilities for each variable
# -----------------------------------------------------------------------

# reweight probabilities according to frequency in the imputed sample
# why are we doing the frequency here now?


test <-complete(mice.out, 1)
imputed_draws <- list()
for(i in 1:5) {
    imputed_draws[[i]] <- complete(mice.out,i)[12607:nrow(test),]
    imputed_draws[[i]] <- cbind(imputed_draws[[i]],fairfax_all_latlong[,c(9:12)])
}

#Drawing from PUMS_person_interest to reweight it by ACS

#DREM
drem_prob <- table(pums_person_interest$DREM)/nrow(pums_person_interest)
acs_DREM_prob_weighted <- acs_DREM_prob
acs_DREM_prob_weighted[,2] <- acs_DREM_prob[,2]/drem_prob[1]
acs_DREM_prob_weighted[,3] <- acs_DREM_prob[,3]/drem_prob[2]
acs_DREM_prob_weighted[,4] <- acs_DREM_prob[,4]/drem_prob[3]


# RACE!
race_prob <- table(pums_person_interest$RAC1P)/nrow(pums_person_interest)
acs_RAC1P_prob_weighted <- acs_RAC1P_prob
acs_RAC1P_prob_weighted[,2] <- acs_RAC1P_prob[,2]/race_prob[1]
acs_RAC1P_prob_weighted[,3] <- acs_RAC1P_prob[,3]/race_prob[2]
acs_RAC1P_prob_weighted[,4] <- acs_RAC1P_prob[,4]/race_prob[3]
acs_RAC1P_prob_weighted[,5] <- acs_RAC1P_prob[,5]/race_prob[4]
acs_RAC1P_prob_weighted[,6] <- acs_RAC1P_prob[,6]/race_prob[5]

# SEX
sex_prob <- table(pums_person_interest$SEX)/nrow(pums_person_interest)
acs_SEX_prob_weighted <- acs_SEX_prob
acs_SEX_prob_weighted[,2] <- acs_SEX_prob[,2]/sex_prob[1]
acs_SEX_prob_weighted[,3] <- acs_SEX_prob[,3]/sex_prob[2]

#AGE
age_prob <- table(pums_person_interest$AGEP)/nrow(pums_person_interest)
acs_AGEP_prob_weighted <- acs_AGEP_prob
acs_AGEP_prob_weighted[,2] <- acs_AGEP_prob[,2]/age_prob[1]
acs_AGEP_prob_weighted[,3] <- acs_AGEP_prob[,3]/age_prob[2]
acs_AGEP_prob_weighted[,4] <- acs_AGEP_prob[,4]/age_prob[3]
acs_AGEP_prob_weighted[,5] <- acs_AGEP_prob[,5]/age_prob[4]

#PINCP
pincp_prob <- table(pums_person_interest$PINCP)/nrow(pums_person_interest)
acs_PINCP2_prob_weighted <- acs_PINCP2_prob
acs_PINCP2_prob_weighted[,2] <- acs_PINCP2_prob[,2]/pincp_prob[1]
acs_PINCP2_prob_weighted[,3] <- acs_PINCP2_prob[,3]/pincp_prob[2]
acs_PINCP2_prob_weighted[,4] <- acs_PINCP2_prob[,4]/pincp_prob[3]
acs_PINCP2_prob_weighted[,5] <- acs_PINCP2_prob[,5]/pincp_prob[4]
acs_PINCP2_prob_weighted[,6] <- acs_PINCP2_prob[,6]/pincp_prob[5]
acs_PINCP2_prob_weighted[,7] <- acs_PINCP2_prob[,7]/pincp_prob[6]
acs_PINCP2_prob_weighted[,8] <- acs_PINCP2_prob[,8]/pincp_prob[7]
acs_PINCP2_prob_weighted[,9] <- acs_PINCP2_prob[,9]/pincp_prob[8]

#PAP
pap_prob <- table(pums_person_interest$PAP)/nrow(pums_person_interest)
acs_PAP2_prob_weighted <- acs_PAP2_prob
acs_PAP2_prob_weighted[,2] <- acs_PAP2_prob[,2]/sex_prob[1]
acs_PAP2_prob_weighted[,3] <- acs_PAP2_prob[,3]/sex_prob[2]



marginal_weights <- matrix(NA,nrow=nrow(imputed_draws[[1]]),ncol=length(imputed_draws))

acs_marginal <- function(zip_codes, values, acs_prob, value_to_column) {
    rownum <- match(zip_codes,acs_prob$zipcode)
    colnum <- value_to_column[values]
    weights <- rep(NA,length(rownum))
    #browser()
    for(i in 1:length(rownum)){ weights[i] <- acs_prob[rownum[i],colnum[i]]
        #print(i)
        #print(unique(values))

    }
    return(weights)
}


# for now, just weight on DREM to make the plot
for(i in 1:length(imputed_draws)){
    # set 'NA' in DREM to 3
    #imputed_draws[[i]]$DREM[is.na(imputed_draws[[i]]$DREM)] <- 3
    # bin age according to breaks
        #imputed_draws[[i]]$AGE_F <- as.numeric( cut(imputed_draws[[i]]$AGEP,breaks=acs_AGE_breaks) )
    # weights are a product of the marginal distributions
        #marginal_weights[,i] <- acs_marginal(imputed_draws[[i]]$ZCTAS,imputed_draws[[i]]$DREM,
                                             #acs_DREM_prob_weighted,value_to_column <- 2:4)
    # original code, product of four marginals:
    marginal_weights[,i] <-
     acs_marginal(imputed_draws[[i]]$ZCTAS,imputed_draws[[i]]$RAC1P,
                   acs_RAC1P_prob_weighted,value_to_column <- c(2,3,5,5,5,4,5,5,6)) *  #You all these value to columns and colums corresponding to Imputation (then, we are matching it to ACS): Length of the c(...) is length of imputation factor!
      acs_marginal(imputed_draws[[i]]$ZCTAS,imputed_draws[[i]]$DREM,
                   acs_DREM_prob_weighted,value_to_column <- 2:4) *
      acs_marginal(imputed_draws[[i]]$ZCTAS,imputed_draws[[i]]$SEX,
                    acs_SEX_prob_weighted,value_to_column <- 2:3) *
      acs_marginal(imputed_draws[[i]]$ZCTAS,imputed_draws[[i]]$AGEP,
                      acs_AGEP_prob_weighted,value_to_column <- 2:5) *
      acs_marginal(imputed_draws[[i]]$ZCTAS, imputed_draws[[i]]$PAP,
                    acs_PAP2_prob_weighted, value_to_column <- 2:3) *
      acs_marginal(imputed_draws[[i]]$ZCTAS, imputed_draws[[i]]$PINCP,
                    acs_PINCP2_prob_weighted, value_to_column <- 2:9)
    #marginal_samp_prob[,i] <- marginal_weights[,i]/sum(marginal_weights[,i])
    if(i%%5==0){print(i)} # show progress
}

save(marginal_weights, file = "~/git/comm_fairfax/data/comm_fairfax/final/MWeight.Rdata")


# are the sampling probabilities stable? bad if one number is much higher than the others
sort(marginal_weights[1,],decreasing = T)
hist(log(marginal_weights[1,]))

# -----------------------------------------------------------------------

# draw samples from the weighted marginal distribution
ndraws <- 1
numdraws <- 5
# imputed_draws_weighted <- list()
# for(i in 1:ndraws){
#     imputed_draws_weighted[[i]] <- imputed_draws[[i]]
# }

# problem: this runs very slowly; speed it up
ind_draw <- matrix(nrow=nrow(imputed_draws[[1]]), ncol=length(imputed_draws[[1]]))

#Fixing code here
for(i in 1:nrow(imputed_draws[[1]])){
    ind_draw[i,] <- sample(size=ndraws, x=1:numdraws, prob=marginal_weights[i,], replace=TRUE)
}

nperson <- nrow(imputed_draws[[1]])
all_imputed_draws <- do.call(rbind,imputed_draws)
# get first imputed sample (eventually want to loop over ind_draw[,i])
ind_draw_all <- (ind_draw[,1]-1)*nperson + (1:nperson)
imputed_draw_weighted <- all_imputed_draws[ind_draw_all,]

save(imputed_draw_weighted, file = "~/git/comm_fairfax/data/comm_fairfax/final/imputed_draw_weight.Rdata")

# clean up
#rm(imputed_draws)
#rm(all_imputed_draws)
save(pums_orig,imputed_draw_weighted,file="sample_draw.RData")

# -----------------------------------------------------------------------
# Plot reweighted sample
# -By ZCTA (ACS)
# -By High School Boundary (reweighted)
# -----------------------------------------------------------------------

# attach values to plot by id to zip.df
dremplot_hs <- imputed_draw_weighted %>% group_by(HighSchool) %>% dplyr::summarize(drem_prob = 100*sum(DREM==1)/n())
dremplot_hs <- dremplot_hs[-26,] # remove NA
names(dremplot_hs)[1] <- "SCHOOL_NAM"

#dremplot_zip <- acs_DREM_prob[,1:2]
#dremplot_zip[,2] <- 100*dremplot_zip[,2]
dremplot_zip <- imputed_draw_weighted %>% group_by(ZCTAS) %>% dplyr::summarize(drem_prob = 100*sum(DREM==1)/n())
names(dremplot_zip)[1] <- "ZCTA5CE10"


library(ggplot2)
library(ggmap)
library(sp)
library(tigris)

ffx_map <- get_map(location=c(-77.7173, 38.5976, -76.8686, 39.0682), source = "google", color = "bw")
ggmap(ffx_map)

#County Shape File
county <- readShapePoly("~sdal/projects/limbo/fairfax_alerts/GISData/Fairfax_County_Border/Fairfax_County_Border.shp",
                        proj4string=CRS('+proj=longlat +ellps=WGS84'))

# here's how I originally created the ZCTA shape file
#zip <- zctas()
#ind_ffx <- which( zip@data$ZCTA5CE10 %in% zipdat$zip )
#zip_ffx <- zip[ind_ffx,]
#save(zip_ffx,file="fairfax_zip_poly.RData")

load("sdal/projects/comm_fairfax/final/fairfax_zip_poly.RData") # zip_ffx
zip <- zip_ffx
zip@data$id <- rownames(zip@data)
zip@data   <- join(zip@data, dremplot_zip, by="ZCTA5CE10")
zip.df     <- fortify(zip)
zip2.df     <- join(zip.df,zip@data, by="id")

#High School Shape File
highSchool <- readShapePoly("~sdal/projects/limbo/fairfax_alerts/GISData/High_School_Pyramids/High_School_Attendance_Areas.shp",
                            proj4string=CRS('+proj=longlat +ellps=WGS84'))
highSchool@data$id <- rownames(highSchool@data)
highSchool@data   <- join(highSchool@data, dremplot_hs, by="SCHOOL_NAM")
highSchool.df     <- fortify(highSchool)
highSchool2.df     <- join(highSchool.df,highSchool@data, by="id")

# sample heat maps for high school, zcta
p_zip <- ggmap(ffx_map) +
    geom_polygon(data = zip2.df, aes(x = long, y = lat, group = group, fill= drem_prob), color = "black") +
    scale_fill_gradient2(low = '#19bd00', mid = '#f5f671', high = '#fd0000', midpoint =2, limits=c(0,5.9),
                         guide=guide_colourbar(title = "Percent")) +
    theme(axis.title.x=element_text(size=16),
          axis.text.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.x=element_blank(),
          axis.ticks.y=element_blank()) +
    xlab("Cognitive Difficulty, Zip Code Tabulated Area")


p_hs <- ggmap(ffx_map) +
    geom_polygon(data = highSchool2.df, aes(x = long, y = lat, group = group, fill= drem_prob), color = "black") +
    scale_fill_gradient2(low = '#19bd00', mid = '#f5f671', high = '#fd0000', midpoint =2, limits=c(0,5.9),
                         guide=guide_colourbar(title = "Percent")) +
    theme(axis.title.x=element_text(size=16),
          axis.text.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.x=element_blank(),
          axis.ticks.y=element_blank()) +
    xlab("Cognitive Difficulty, High School Area")

png("drem_zip.png",width=800,height=800)
p_zip
dev.off()

png("drem_hs.png",width=800,height=800)
p_hs
dev.off()

# save(p_zip,p_hs,file="poster_plots.RData")




