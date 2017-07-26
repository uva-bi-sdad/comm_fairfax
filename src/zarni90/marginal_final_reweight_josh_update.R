# do 'easy' imputation
library(dplyr)

setwd("~git/comm_fairfax/src/joshg22/")

synth_fairfax <- read.csv("~sdal/projects/comm_fairfax/final/fairfax_latlong.csv")[,-1]

pums_orig <- read.csv("~sdal/projects/comm_fairfax/final/pums_ffx.csv")
pums_orig <- pums_orig %>% dplyr::select(SERIALNO,PUMA,PWGTP,PINCP,DREM,ENG,PAP,RAC1P,SEX,AGEP)

pums_orig$DREM[is.na(pums_orig$DREM)] <- 3 # set 'under 5' to 3

numdraws <- 100

# for each draw, resample rows from pums_orig into synth_fairfax
# sample with weights given by PWGTP (person weights in PUMS)

imputed_draws <- list()
for(i in 1:numdraws){
    imputed_draws[[i]] <- synth_fairfax
    imputed_draws[[i]][,1:7] <- pums_orig[sample(1:nrow(pums_orig),size=nrow(synth_fairfax),replace = TRUE,prob=pums_orig$PWGTP),4:10]
}

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
zipdat <- as.data.frame(table(synth_fairfax$ZCTAS))
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
acs_RAC1P3 <- acs_RAC1P2
acs_RAC1P3[ acs_RAC1P3 < 100 ] <- 100
acs_RAC1P_prob <- acs_RAC1P3[2:ncol(acs_RAC1P3)]/rowSums(acs_RAC1P3[2:ncol(acs_RAC1P3)])
acs_RAC1P_prob <- cbind(zipcode=acs_RAC1P$zipcode, acs_RAC1P_prob)
# clean up
rm(acs_RAC1P2,acs_RAC1P3)


acs_SEX <- zip_matcher("B01001",zipdat$zip)
# 1=male, 2=female
acs_SEX2 <- data.frame(zipcode=acs_SEX$zipcode, male=acs_SEX$Sex.by.Age..Male.,
                       female=acs_SEX$Sex.by.Age..Female.)
acs_SEX_prob <- acs_SEX2[2:ncol(acs_SEX2)]/rowSums(acs_SEX2[2:ncol(acs_SEX2)])
acs_SEX_prob <- cbind(zipcode=acs_SEX$zipcode, acs_SEX_prob)
# clean up
rm(acs_SEX2)


acs_AGEP2 <- data.frame(zipcode=acs_SEX$zipcode,
                        age0_25=rowSums( acs_SEX[,c(4:11,28:35)]),
                        age25_50=rowSums( acs_SEX[,c(12:16,36:40)] ),
                        age50_75=rowSums( acs_SEX[,c(17:23,41:47)] ),
                        age75_up=rowSums( acs_SEX[,c(24:26,48:50)] ) )
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
acs_DREM_prob <- acs_DREM2[2:ncol(acs_DREM2)]/rowSums(acs_DREM2[2:ncol(acs_DREM2)])
acs_DREM_prob <- cbind(zipcode=acs_DREM$zipcode, acs_DREM_prob)
# clean up
rm(acs_DREM2)

# match on these four for now; but we could also add PAP, PINCP

# -----------------------------------------------------------------------
# draw imputed samples from the marginal distribution (independantly for each person)
# marginal weights are the product of marginal probabilities for each variable
# -----------------------------------------------------------------------

# reweight probabilities according to frequency in the imputed sample
drem_prob <- table(imputed_draws[[1]]$DREM)/nrow(imputed_draws[[1]])
acs_DREM_prob_weighted <- acs_DREM_prob
acs_DREM_prob_weighted[,2] <- acs_DREM_prob[,2]/drem_prob[1]
acs_DREM_prob_weighted[,3] <- acs_DREM_prob[,3]/drem_prob[2]
acs_DREM_prob_weighted[,4] <- acs_DREM_prob[,4]/drem_prob[3]

# TO DO: create reweighted probabilities for the rest of the variables!

marginal_weights <- matrix(NA,nrow=nrow(imputed_draws[[1]]),ncol=length(imputed_draws))

acs_marginal <- function(zip_codes, values, acs_prob, value_to_column) {
    rownum <- match(zip_codes,acs_prob$zipcode)
    colnum <- value_to_column[values]
    weights <- rep(NA,length(rownum))
    for(i in 1:length(rownum)){ weights[i] <- acs_prob[rownum[i],colnum[i]] }
    return(weights)
}

# for now, just weight on DREM to make the plot
for(i in 1:length(imputed_draws)){
    # set 'NA' in DREM to 3
    imputed_draws[[i]]$DREM[is.na(imputed_draws[[i]]$DREM)] <- 3
    # bin age according to breaks
    imputed_draws[[i]]$AGE_F <- as.numeric( cut(imputed_draws[[i]]$AGEP,breaks=acs_AGE_breaks) )
    # weights are a product of the marginal distributions
    marginal_weights[,i] <- acs_marginal(imputed_draws[[i]]$ZCTAS,imputed_draws[[i]]$DREM,
                                         acs_DREM_prob_weighted,value_to_column <- 2:4)
    # original code, product of four marginals:
    #marginal_weights[,i] <-
    #  acs_marginal(imputed_draws[[i]]$ZCTAS,imputed_draws[[i]]$RAC1P,
    #               acs_RAC1P_prob_weighted,value_to_column <- c(2,3,5,5,5,4,5,5,6)) *
    #  acs_marginal(imputed_draws[[i]]$ZCTAS,imputed_draws[[i]]$DREM,
    #               acs_DREM_prob_weighted,value_to_column <- 2:4) *
    #  acs_marginal(imputed_draws[[i]]$ZCTAS,imputed_draws[[i]]$SEX,
    #               acs_SEX_prob_weighted,value_to_column <- 2:3) *
    #  acs_marginal(imputed_draws[[i]]$ZCTAS,imputed_draws[[i]]$AGE_F,
    #               acs_AGEP_prob_weighted,value_to_column <- 2:5)
    #marginal_samp_prob[,i] <- marginal_weights[,i]/sum(marginal_weights[,i])
    if(i%%5==0){print(i)} # show progress
}

# are the sampling probabilities stable? bad if one number is much higher than the others
sort(marginal_weights[1,],decreasing = T)
hist(log(marginal_weights[1,]))

# -----------------------------------------------------------------------

# draw samples from the weighted marginal distribution
ndraws <- 1

imputed_draws_weighted <- list()
for(i in 1:ndraws){
    imputed_draws_weighted[[i]] <- imputed_draws[[1]]
}

# problem: this runs very slowly; speed it up
ind_draw <- matrix(nrow=nrow(imputed_draws[[1]]), ncol=ndraws)
for(i in 1:nrow(imputed_draws[[1]])){
    ind_draw[i,] <- sample(size=ndraws, x=1:numdraws, prob=marginal_weights[i,], replace=TRUE)
}

nperson <- nrow(imputed_draws[[1]])
all_imputed_draws <- do.call(rbind,imputed_draws)
# get first imputed sample (eventually want to loop over ind_draw[,i])
ind_draw_all <- (ind_draw[,1]-1)*nperson + (1:nperson)
imputed_draw_weighted <- all_imputed_draws[ind_draw_all,]

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




