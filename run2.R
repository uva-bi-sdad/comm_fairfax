.libPaths(c('/rpkgs/', '/home/zarni90/R/x86_64-pc-linux-gnu-library/3.3/'))

library(mice)
library(dplyr)

pums_combined <- rio::import("~/git/comm_fairfax/data/comm_fairfax/working/pums_combined.csv")
names(pums_combined)
pums_combined <- pums_combined[-1]
ncol(pums_combined)
pums_combined$RAC1P <- as.factor(pums_combined$RAC1P)
pums_combined$SEX <- as.factor(pums_combined$SEX)
pums_combined$DREM <- as.factor(pums_combined$DREM)
pums_combined$ENG <- as.factor(pums_combined$ENG)
numdraws <- 10
niter <- 15
mice.out <- mice(data=pums_combined%>% dplyr::select(RAC1P,SEX,AGEP,DREM,PINCP, PAP, ENG), m=numdraws,maxit = niter,
                 method=c("polyreg","logreg","norm","polyreg","norm", "norm", "polyreg"), seed = 1234)

setwd("~/git/comm_fairfax/data/comm_fairfax/working/")
save(mice.out, file = "miceoutput3.Rdata")
