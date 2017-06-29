# impute for Arlington renters:
#   household income, #bedrooms (partial), rent
# observed in CoreLogic tax records:
#   water flag, #bedrooms (partial)

# imputation methods:
# Water Flag        logistic regression (logreg)
# Bedrooms (0-4)    polytomous logistic regression (polyreg)
# Household Income  Bayesian linear regression (norm); take square root transformation
# Rent              Bayesian linear regression (norm)
# Year Built        Bayesian linear regression (norm)

library(ggplot2)
library(mice)
library(dplyr)

setwd("/home/sdal/projects/hud_census/analysis/Josh/synthetic population/person variables/")

# read in prepared data
load("cleaned_renters.RData")

# bedroom_marginal: ACS block group distribution on the number of bedrooms
# income_marignal:  ACS block group distribution of household income
# rent_marginal:    ACS block group distribution of monthly rent
# CLdata:           Arlington selected housing data
# PUMS:             Arlington 2014 PUMS data

#Zarni's Remarks
#How do you decide what type of regression to use for which model?
#When you have PUMS: you are saying PUMS housing I believe and that one is complete?
#CLdata : selected tax clear logic data
#Line 48-49: What are HINCP and RNTP?
#mice.out is where imputation is happening.
#So you are imputing both CL and PUMS
#What is the For Loop draws doing?

# -----------------------------------------------------------------------
# topcode #bedrooms in PUMS to 4; this is the max in CoreLogic
# -----------------------------------------------------------------------

PUMS$BDSP[PUMS$BDSP > 4] <- 4

# -----------------------------------------------------------------------
# generate many conditional samples with mice
# -----------------------------------------------------------------------

# create combined data frame
CLdata$source <- "CL"
CLdata$HINCP <- NA
CLdata$RNTP <- NA

PUMS$source <- "PUMS"
PUMS$BlockGroup <- NA
PUMS$WaterFlag <- NA

CL_PUMS <- rbind( CLdata %>% select(HINCP,RNTP,BDSP=BEDROOMS,WaterFlag,YBL=YEAR.BUILT,BlockGroup,source),
                  PUMS %>% select(HINCP,RNTP,BDSP,WaterFlag,YBL,BlockGroup,source) )
CL_PUMS$sqrtHINCP <- sqrt(CL_PUMS$HINCP)
CL_PUMS$BDSP <- as.factor(CL_PUMS$BDSP)
CL_PUMS$WaterFlag <- as.factor(CL_PUMS$WaterFlag)

# impute using mice over m replicates
numdraws <- 100
mice.out <- mice(data=CL_PUMS%>%select(sqrtHINCP,RNTP,BDSP,WaterFlag,YBL), m=numdraws,
                 method=c("norm","norm","polyreg","logreg","norm"))

# get an imputed draw from the synthetic universe for each variable
CLdraws_list <- list() # draws for only imputed CL
CLdraws_full_list <- list() # draws for both imputed CL and imputed PUMS

for(i in 1:numdraws) {
    CLdraws_list[[i]] <- CL_PUMS
    CLdraws_list[[i]]$sqrtHINCP[is.na(CLdraws_list[[i]]$sqrtHINCP)] <- mice.out$imp$sqrtHINCP[,i]
    CLdraws_list[[i]]$RNTP[is.na(CLdraws_list[[i]]$RNTP)] <- mice.out$imp$RNTP[,i]
    CLdraws_list[[i]]$BDSP[is.na(CLdraws_list[[i]]$BDSP)] <- mice.out$imp$BDSP[,i]
    CLdraws_list[[i]]$WaterFlag[is.na(CLdraws_list[[i]]$WaterFlag)] <- mice.out$imp$WaterFlag[,i]
    CLdraws_list[[i]]$YBL[is.na(CLdraws_list[[i]]$YBL)] <- mice.out$imp$YBL[,i]
    # restrict to positive rent, income
    CLdraws_list[[i]]$sqrtHINCP[CLdraws_list[[i]]$sqrtHINCP < 0] <- 0
    CLdraws_list[[i]]$RNTP[CLdraws_list[[i]]$RNTP < 0] <- 0
    # restrict year built to no later than 2013
    CLdraws_list[[i]]$YBL[CLdraws_list[[i]]$YBL > 2013] <- 2013
    CLdraws_list[[i]]$HINCP <- CLdraws_list[[i]]$sqrtHINCP^2
    # turn factors into numeric
    CLdraws_list[[i]]$BDSP <- as.numeric(paste(CLdraws_list[[i]]$BDSP))
    CLdraws_list[[i]]$WaterFlag <- as.numeric(paste(CLdraws_list[[i]]$WaterFlag))

    CLdraws_full_list[[i]] <- CLdraws_list[[i]]
    CLdraws_list[[i]] <- CLdraws_list[[i]] %>% filter(source=="CL")
}

CLdraw <- CLdraws_full_list[[1]]
CLdraw2 <- CLdraws_list[[1]]

# -----------------------------------------------------------------------
# plot conditional distributions based on one draw
# -----------------------------------------------------------------------

# compare imputed income vs rent
png("fig/renter_mice_income_rent.png",width=500,height=500)
par(mar=c(5,5,1,1))
plot(CLdraw2$sqrtHINCP,CLdraw2$RNTP,pch=20,cex=0.5,xlab="sqrt(Household Income)",ylab="Monthly Rent",cex.lab=2,col="grey25",cex.axis=1.5)
points(sqrt(PUMS$HINCP),PUMS$RNTP,col="red2",pch=20,cex=0.5)
legend(x="topleft",legend=c("Imputed","PUMS"),col=c("grey25","red2"),pch=c(20,20),cex=1.5)
dev.off()

# compare imputed income vs year built
png("fig/renter_mice_income_year.png",width=500,height=500)
par(mar=c(5,5,1,1))
plot(CLdraw2$YBL,CLdraw2$sqrtHINCP,pch=20,cex=0.5,xlab="Year Built",ylab="sqrt(Household Income)",cex.lab=2,col="grey25",cex.axis=1.5)
points(PUMS$YBL,sqrt(PUMS$HINCP),col="red2",pch=20,cex=0.5)
legend(x="topleft",legend=c("Imputed","PUMS"),col=c("grey25","red2"),pch=c(20,20),cex=1.5)
dev.off()

# compare rent vs year built
png("fig/renter_mice_rent_year.png",width=500,height=500)
par(mar=c(5,5,1,1))
plot(CLdraw2$YBL,CLdraw2$RNTP,pch=20,cex=0.5,xlab="Year Built",ylab="Monthly Rent",cex.lab=2,col="grey25",cex.axis=1.5)
points(PUMS$YBL,PUMS$RNTP,col="red2",pch=20,cex=0.5)
legend(x="topleft",legend=c("Imputed","PUMS"),col=c("grey25","red2"),pch=c(20,20),cex=1.5)
dev.off()

# compare imputed #bedrooms vs rent (bar graph)
png("fig/renter_mice_bedrooms_rent.png",width=500,height=500)
ggplot(aes(y = RNTP, x = BDSP, fill = source), data = CLdraw) + geom_boxplot() +
    scale_fill_manual(values=c("grey25","red2"),
                      labels=c("Imputed","PUMS"),
                      name=element_blank()) +
    labs(x="Bedrooms",y="Monthly Rent") +
    theme(axis.title.x=element_text(size=20),
          axis.title.y=element_text(size=20),
          axis.text.x=element_text(size=16),
          axis.text.y=element_text(size=16),
          legend.text=element_text(size=16))
dev.off()

# compare imputed #bedrooms vs income (bar graph)
png("fig/renter_mice_bedrooms_income.png",width=500,height=500)
ggplot(aes(y = sqrtHINCP, x = BDSP, fill = source), data = CLdraw) + geom_boxplot() +
    scale_fill_manual(values=c("grey25","red2"),
                      labels=c("Imputed","PUMS"),
                      name=element_blank()) +
    labs(x="Bedrooms",y="sqrt(Household Income)") +
    theme(axis.title.x=element_text(size=20),
          axis.title.y=element_text(size=20),
          axis.text.x=element_text(size=16),
          axis.text.y=element_text(size=16),
          legend.text=element_text(size=16))
dev.off()

# compare imputed income vs water flag, imputed rent vs water flag
png("fig/renter_mice_water_income.png",width=500,height=500)
ggplot(aes(y = sqrtHINCP, x = WaterFlag, fill = source), data = CLdraw2) + geom_boxplot() +
    scale_fill_manual(values=c("grey25"),
                      guide=FALSE) +
    labs(x="Water Flag",y="sqrt(Household Income)") +
    theme(axis.title.x=element_text(size=20),
          axis.title.y=element_text(size=20),
          axis.text.x=element_text(size=16),
          axis.text.y=element_text(size=16),
          legend.text=element_text(size=16))
dev.off()

png("fig/renter_mice_water_rent.png",width=500,height=500)
ggplot(aes(y = RNTP, x = WaterFlag, fill = source), data = CLdraw2) + geom_boxplot() +
    scale_fill_manual(values=c("grey25"),
                      guide=FALSE) +
    labs(x="Water Flag",y="Monthly Rent") +
    theme(axis.title.x=element_text(size=20),
          axis.title.y=element_text(size=20),
          axis.text.x=element_text(size=16),
          axis.text.y=element_text(size=16),
          legend.text=element_text(size=16))
dev.off()


# -----------------------------------------------------------------------
# marginal distribution for income; piecewise uniform w/ exponential tails
# -----------------------------------------------------------------------

# use counts from ACS table to estmiate a piecewise uniform distribution by blockgroup
# fit an exponential tail when income is $200,000+
income_marginal$BlockGroup <-  as.numeric(substr(income_marginal$Id2,6,12))
income_marginal <- cbind(BlockGroup=income_marginal$BlockGroup,income_marginal[,4:19])
income_marginal <- income_marginal %>% filter(BlockGroup != 9801001) # filter out blockgroup with 0 observations
# aggregate counts every 25k
income_marginal <- income_marginal %>% transmute(BlockGroup,"0-25k"=Estimate..Total.....10.000.to..14.999+Estimate..Total....Less.than..10.000+
                                                     Estimate..Total.....15.000.to..19.999+Estimate..Total.....20.000.to..24.999,
                                                 "25-50k"=Estimate..Total.....25.000.to..29.999+Estimate..Total.....30.000.to..34.999+
                                                     Estimate..Total.....35.000.to..39.999+Estimate..Total.....40.000.to..44.999+Estimate..Total.....45.000.to..49.999,
                                                 "50-75k"=Estimate..Total.....50.000.to..59.999+Estimate..Total.....60.000.to..74.999,
                                                 "75-100k"=Estimate..Total.....75.000.to..99.999,
                                                 "100-125k"=Estimate..Total.....100.000.to..124.999,
                                                 "125-150k"=Estimate..Total.....125.000.to..149.999,
                                                 "150-200k"=Estimate..Total.....150.000.to..199.999,
                                                 "200+k"=Estimate..Total.....200.000.or.more)
breaks <- c(-1,25000,50000,75000,100000,125000,150000,200000)

ncols <- ncol(income_marginal)
z <- 0.1 # zero inflated; uniform from 0 to 2e5 by z/2, exponential tails by z/2 to capture zero counts in the ACS

# area under piecewise uniform (histogram) density: percent of observations under 200,000
area_hist <- (1-z/2)*rowSums(income_marginal[,2:(ncols-1)])/rowSums(income_marginal[,2:ncols])
# area under exponential tail: percent over 200,000
area_exp <- (1-z/2)*income_marginal[,ncols]/rowSums(income_marginal[,2:ncols]) + z/2

widths <- diff(breaks)
marginal_uniform_density <- (1-z/2)*income_marginal[,2:(ncols-1)]/rowSums(income_marginal[,2:ncols])
marginal_uniform_density <- sweep(marginal_uniform_density,2,z/2*widths/sum(widths),FUN = "+") # add constant uniform
marginal_uniform_density_mat <- as.data.frame((as.matrix(marginal_uniform_density) %*% diag(1/widths)))
names(marginal_uniform_density_mat) <- names(marginal_uniform_density)
marginal_uniform_density_mat <- cbind(BlockGroup=income_marginal$BlockGroup,marginal_uniform_density_mat)

# next, fit exponential tail for values > 200,000
samp_income <- PUMS$HINCP[PUMS$HINCP > 200000 & !is.na(PUMS$HINCP)] - 200000
# use the MLE for an exponential distribution, 1/x-bar
mle_lambda <- 1/mean(samp_income)
# plot the fitted exponential tail
hist(samp_income+2e5, freq=FALSE,breaks=30,xlab="Income",main="PUMS Fitted Exponential Tail")
xs <- seq(2e5,8e5,length=100)
lines(xs, mle_lambda*exp(-mle_lambda*(xs-2e5)),col=2,lwd=2)

# write a function for the marginal distribution for each blockgroup; make plots
# vectorize this function (vector of inputs for income and blockgroup)
income_marginal_dist <- function(income,blockgroup){
    # get the piecewise uniform density, area_exp corresponding to this blockgroup
    ind <- match(blockgroup,marginal_uniform_density_mat$BlockGroup)
    uniform_density_bg <- as.matrix(marginal_uniform_density_mat[ind,-1])
    area_exp_bg <- area_exp[ind]

    density_out <- rep(NA,length(income))
    # find the range income falls into, return uniform density
    breaks <- c(-1,25000,50000,75000,100000,125000,150000,200000)
    cut_ind <- cut(income,breaks,labels=1:(length(breaks)-1))
    for(i in 1:length(density_out)){
        density_out[i] <- uniform_density_bg[i,cut_ind[i]]
    }
    # restricted to positive income
    density_out[income < 0] <- 0
    # exponential tail
    ind_highincome <- which(income >= 2e5)
    density_out[ind_highincome] <- area_exp_bg[ind_highincome] * mle_lambda * exp(-mle_lambda*(income[ind_highincome]-2e5))

    return(density_out)
}

#test <- marginal_dist(income=runif(n=nrow(CLdata),min=0,max=4e5),blockgroup=CLdata$BlockGroup)
#sum(log(test)) # loglikelihood

# -----------------------------------------------------------------------
# plot the marginal distributions by blockgroup for a sample of 20
# -----------------------------------------------------------------------

par(mfrow=c(5,4))
par(mar=c(3,3,1,1))
samp_bg <- sort(sample(marginal_uniform_density_mat$BlockGroup,20))
for(i in 1:20){
    xs <- seq(0,4e5,length=400)
    bg <- samp_bg[i]
    ys <- marginal_dist(xs,rep(bg,length(xs)))
    plot(xs,ys,type="l",xlab="Income",ylab="Density",main="",ylim=c(0,max(ys)))
    text(paste(bg),x=3.3e5,y=max(ys)*.75,font=2,cex=1.5)
}

# -----------------------------------------------------------------------
# marginal distribution for bedrooms (renters); multinomial
# -----------------------------------------------------------------------

z <- 0.05 # zero inflation to capture zero counts in ACS
bedroom_marginal$BlockGroup <-  as.numeric(substr(bedroom_marginal$Id2,6,12))
bedroom_marginal <- cbind(BlockGroup=bedroom_marginal$BlockGroup,bedroom_marginal[,12:17])
bedroom_marginal <- bedroom_marginal %>% filter(BlockGroup != 9801001) # filter out blockgroup with 0 observations
bedroom_marginal <- bedroom_marginal %>% transmute(BlockGroup,
                                                   "0"=Estimate..Renter.occupied....No.bedroom,
                                                   "1"=Estimate..Renter.occupied....1.bedroom,
                                                   "2"=Estimate..Renter.occupied....2.bedrooms,
                                                   "3"=Estimate..Renter.occupied....3.bedrooms,
                                                   "4+"=Estimate..Renter.occupied....4.bedrooms+Estimate..Renter.occupied....5.or.more.bedrooms)
bedroom_multi_prob <- z + (1-5*z)*bedroom_marginal[,2:6]/rowSums(bedroom_marginal[,2:6])
bedroom_multi_prob[is.na(bedroom_multi_prob)] <- 1/5 # flat probability if no ACS data available
bedroom_multi_prob <- cbind(BlockGroup=bedroom_marginal$BlockGroup,bedroom_multi_prob)

# marginal distribution for each blockgroup; vector of inputs for bedrooms and blockgroup
bedroom_marginal_dist <- function(bedrooms,blockgroup){
    # get the piecewise uniform density, area_exp corresponding to this blockgroup
    ind <- match(blockgroup,bedroom_multi_prob$BlockGroup)
    multi_mat <- as.matrix(bedroom_multi_prob[ind,-1])
    multi_mat[is.na(multi_mat)] <- 1/5 # flat probability if no ACS data available

    density_out <- rep(NA,length(bedrooms))
    # find the range income falls into, return uniform density
    breaks <- c(-.5,.5,1.5,2.5,3.5,Inf)
    cut_ind <- as.numeric( cut(bedrooms,breaks,labels=1:(length(breaks)-1)) )
    for(i in 1:length(density_out)){
        density_out[i] <- multi_mat[i,cut_ind[i]]
    }
    return(density_out)
}

#test <- bedroom_marginal_dist(bedrooms=sample(x=0:5,size=nrow(CLdata),replace=TRUE),blockgroup=CLdata$BlockGroup)
#sum(log(test)) # loglikelihood

# -----------------------------------------------------------------------
# marginal distribution for rent; multinomial (can't fit exponential tail since no data for rent > $2000)
# -----------------------------------------------------------------------

z <- 0.05 # zero inflation to capture zero counts in ACS
rent_marginal$BlockGroup <-  as.numeric(substr(rent_marginal$Id2,6,12))
rent_marginal <- cbind(BlockGroup=rent_marginal$BlockGroup,rent_marginal[,5:25])
rent_marginal <- rent_marginal %>% filter(BlockGroup != 9801001) # filter out blockgroup with 0 observations
rent_marginal <- rent_marginal %>% transmute(BlockGroup,
                                             "0-500"=Estimate..With.cash.rent....Less.than..100+Estimate..With.cash.rent.....100.to..149+Estimate..With.cash.rent.....150.to..199+
                                                 Estimate..With.cash.rent.....200.to..249+Estimate..With.cash.rent.....250.to..299+Estimate..With.cash.rent.....300.to..349+
                                                 Estimate..With.cash.rent.....350.to..399+Estimate..With.cash.rent.....400.to..449+Estimate..With.cash.rent.....450.to..499,
                                             "500-1000"=Estimate..With.cash.rent.....500.to..549+Estimate..With.cash.rent.....550.to..599+Estimate..With.cash.rent.....600.to..649+
                                                 Estimate..With.cash.rent.....650.to..699+Estimate..With.cash.rent.....700.to..749+Estimate..With.cash.rent.....750.to..799+
                                                 Estimate..With.cash.rent.....800.to..899+Estimate..With.cash.rent.....900.to..999,
                                             "1000-1500"=Estimate..With.cash.rent.....1.000.to..1.249+Estimate..With.cash.rent.....1.250.to..1.499,
                                             "1500-2000"=Estimate..With.cash.rent.....1.500.to..1.999,
                                             "2000+"=Estimate..With.cash.rent.....2.000.or.more)
rent_multi_prob <- z + (1-5*z)*rent_marginal[,2:6]/rowSums(rent_marginal[,2:6])
rent_multi_prob[is.na(rent_multi_prob)] <- 1/5 # flat probability if no ACS data available
rent_multi_prob <- cbind(BlockGroup=rent_marginal$BlockGroup,rent_multi_prob)

# marginal distribution for each blockgroup; vector of inputs for rents and blockgroup
rent_marginal_dist <- function(rent,blockgroup){
    # get the piecewise uniform density, area_exp corresponding to this blockgroup
    ind <- match(blockgroup,rent_multi_prob$BlockGroup)
    multi_mat <- as.matrix(rent_multi_prob[ind,-1])
    multi_mat[is.na(multi_mat)] <- 1/5 # flat probability if no ACS data available

    density_out <- rep(NA,length(rent))
    # find the range income falls into, return uniform density
    breaks <- c(-Inf,500,1000,1500,2000,Inf)
    cut_ind <- as.numeric( cut(rent,breaks,labels=1:(length(breaks)-1)) )
    for(i in 1:length(density_out)){
        density_out[i] <- multi_mat[i,cut_ind[i]]
    }
    return(density_out)
}

#test <- bedroom_marginal_dist(bedrooms=runif(min=0,max=2500,n=nrow(CLdata)),blockgroup=CLdata$BlockGroup)
#sum(log(test)) # loglikelihood

# -----------------------------------------------------------------------
# draw imputed samples from the marginal distribution (independantly for each household)
# marginal weights are the product of marginal probabilities for income, rent, bedrooms
# -----------------------------------------------------------------------

marginal_weights <- matrix(NA,nrow=nrow(CLdraws_list[[1]]),ncol=length(CLdraws_list))
marginal_samp_prob <- matrix(NA,nrow=nrow(CLdraws_list[[1]]),ncol=length(CLdraws_list))

for(i in 1:length(CLdraws_list)){
    marginal_weights[,i] <- income_marginal_dist(income=CLdraws_list[[i]]$HINCP,blockgroup=CLdata$BlockGroup) *
        bedroom_marginal_dist(bedrooms=CLdraws_list[[i]]$BDSP,blockgroup=CLdata$BlockGroup) *
        rent_marginal_dist(rent=CLdraws_list[[i]]$RNTP,blockgroup=CLdata$BlockGroup)
    marginal_samp_prob[,i] <- marginal_weights[,i]/sum(marginal_weights[,i])
    if(i%%50==0){print(i)}
}

sort(marginal_samp_prob[,1],decreasing = T)[1:10]
sort(marginal_samp_prob[,1],decreasing = F)[1:10]
max(marginal_samp_prob[,1])/min(marginal_samp_prob[,1])

# draw samples from the weighted marginal distribution
# (samples are a list of data frames)
ndraws <- 1

CLdraws_weighted <- list()
for(i in 1:ndraws){
    CLdraws_weighted[[i]] <- CLdraws_list[[1]]
}
# take each *row* from the sampled draw
for(i in 1:nrow(CLdraws_weighted[[i]])){
    ind_draw <- sample(size=ndraws, x=1:numdraws, prob=marginal_samp_prob[i,], replace=TRUE)
    for(j in 1:ndraws){
        CLdraws_weighted[[j]][i,] <- CLdraws_list[[ind_draw[j]]][i,]
    }
}

save.image("renter_imputed.RData")

# -----------------------------------------------------------------------
# plots vs marginal distributions by blockgroup
# -----------------------------------------------------------------------

# -----------------------------------------------------------------------
# plot imputed joint income for a single draw by blockgroup (histogram) vs marginal distributions
# -----------------------------------------------------------------------

income_draws_plot <- as.data.frame(cbind(income=CLdraws_weighted[[1]]$sqrtHINCP^2,BlockGroup=CLdata$BlockGroup))

png("renters_income_marginal.png",width=800,height=600)
par(mfrow=c(5,4))
par(mar=c(3,3,1,1))
samp_bg <- sort(sample(unique(income_draws_plot$BlockGroup),20))
for(i in 1:20){
    xs <- seq(0,4e5,length=400)
    bg <- samp_bg[i]
    ys <- income_marginal_dist(xs,rep(bg,length(xs)))
    ymax <- max( c(ys,hist( (income_draws_plot %>% filter(BlockGroup==bg))[,1], plot=FALSE )$density ) )
    hist( (income_draws_plot %>% filter(BlockGroup==bg))[,1], add=FALSE, freq = FALSE, xlim=c(0,4e5),ylim=c(0,ymax*1.1),
          col="lightblue",border="blue",main="",xlab="Income",ylab="Density",
          breaks = c(breaks,2.5e5,3e5,3.5e5,4e5,Inf) )
    lines(xs,ys,type="l",xlab="Income",ylab="Density",main="",lwd=2,col="red")
    text(paste(bg),x=3.3e5,y=max(ys)*.75,font=2,cex=1.5)
}
dev.off()

# -----------------------------------------------------------------------
# plot imputed conditional income (from mice) for a single draw by blockgroup (histogram) vs marginal distributions
# -----------------------------------------------------------------------

income_conditional_plot <- as.data.frame(cbind(income=CLdraws_list[[1]]$sqrtHINCP^2,BlockGroup=CLdata$BlockGroup))

png("renters_income_marginal_mice_draw.png",width=800,height=600)
par(mfrow=c(5,4))
par(mar=c(3,3,1,1))
#samp_bg <- sort(sample(unique(income_draws_plot$BlockGroup),20))
for(i in 1:20){
    xs <- seq(0,4e5,length=400)
    bg <- samp_bg[i]
    ys <- income_marginal_dist(xs,rep(bg,length(xs)))
    ymax <- max( c(ys,hist( (income_draws_plot %>% filter(BlockGroup==bg))[,1], plot=FALSE )$density ) )
    hist( (income_conditional_plot %>% filter(BlockGroup==bg))[,1], add=FALSE, freq = FALSE, xlim=c(0,4e5),ylim=c(0,ymax*1.1),
          col="green",border="darkgreen",main="",xlab="Income",ylab="Density",
          breaks = c(breaks,2.5e5,3e5,3.5e5,4e5,Inf) )
    lines(xs,ys,type="l",xlab="Income",ylab="Density",main="",lwd=2,col="red")
    text(paste(bg),x=3.3e5,y=max(ys)*.75,font=2,cex=1.5)
}
dev.off()


# -----------------------------------------------------------------------
# Affordable Housing:
#   plot 12*RNTP/HINCP (from 0 to 1)
#   summary shaded plot by blockgroup
# -----------------------------------------------------------------------

library(maptools) # Tools for handling spatial objects

geodat<-readShapePoly("/home/sdal/projects/census/data/Housing/Arlington_GIS_Data/3_TIDY/Virginia Block Groups 2013/tl_2013_51_bg.shp",
                      proj4string=CRS('+proj=longlat +datum=NAD83'))
geodat1<-geodat[which(geodat$COUNTYFP=="013"),] # Arlington County
plot(geodat1)

# note: need to add lat, long to get individual points (but this isn't useful; many units in one apartment)

#points(CLdata$LONGITUDE,CLdata$LATITUDE,pch=20,cex=0.1,col=CLdata$valcolor)
affordable_housing <- data.frame(BlockGroup = CLdata$BlockGroup, income_percent = CLdraws_weighted[[1]]$RNTP*12/CLdraws_weighted[[1]]$HINCP)
affordable_housing$income_percent[affordable_housing$income_percent > 1] <- 1

#hist(affordable_housing$income_percent,xlab="Rent Proportion of Household Income",freq=FALSE,main="")
affordable_housing_bg <- affordable_housing %>% group_by(BlockGroup) %>% summarize(median_income_percent = median(income_percent,na.rm=TRUE))
hist(affordable_housing_bg$median_income_percent)


CLrange <- read.csv("/home/sdal/projects/hud_census/data/Merged_Data/Merged_Data_Arlington_County_2013.csv")
x.lim <- range(CLrange$LONGITUDE,na.rm=T)
y.lim <- range(CLrange$LATITUDE,na.rm=T)

marginal_col <- data.frame(BlockGroup = as.numeric(substr(geodat1$GEOID,6,nchar(as.character(geodat1$GEOID)))))
marginal_col2 <- marginal_col %>% left_join(affordable_housing_bg,by="BlockGroup")

colfunc <- colorRampPalette(c("lightblue", "red"))
test <- as.numeric(cut(marginal_col2$median_income_percent, breaks = 10))
marginal_col2$incomecolor <- colfunc(10)[test]
#marginal_col2$incomecolor[is.na(marginal_col2$incomecolor)] <- colfunc(10)[3]

png("affordable_housing_blockgroup.png",width=600,height=600)
plot(geodat1,col=marginal_col2$incomecolor)
legend_image <- as.raster(matrix(colfunc(10), ncol=1))
rasterImage(legend_image, x.lim[1]+diff(x.lim)*.05, y.lim[1]+diff(y.lim)*.4, x.lim[1]+diff(x.lim)*.1,y.lim[1]+diff(y.lim)*0)
text(x.lim[1]+diff(x.lim)*.16, y = seq(y.lim[1]+diff(y.lim)*0,y.lim[1]+diff(y.lim)*.4,l=5),
     labels = c(0,20,40,60,80))
dev.off()

