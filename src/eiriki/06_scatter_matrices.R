#doing a multiple scatterplot for regression on aggregates
library(car)
setwd('~/git/lab/comm_fairfax/')
HS_Pyramid_Report <- read_excel("data/comm_fairfax/original/2015 Supplemental Analysis by Pyramid Report__GIS.xlsx",
                                sheet = "8-10-12 Results by Pyramid")
HS_Pyramid_Report <- HS_Pyramid_Report[-1,]
colnames(HS_Pyramid_Report) <- HS_Pyramid_Report[1,]
HS_Pyramid_Report <- HS_Pyramid_Report[-1,]
#Making the new data table for the specific catergories
HS_Pyramid_Report <- HS_Pyramid_Report[,c(1,2,3,
                                           90,91,92,
                                           93,94,95,
                                           105,72,73,101,106,134,135,136,
                                           20,21,126,
                                           88,89,138,140,67,48,52,61)]
#make all columns numerics
HS_Pyramid_Report[,4:28] <- sapply(HS_Pyramid_Report[4:28], as.numeric)

#example scatterplot matrix for the four mental health variables
scatterplotMatrix(~Depressive_Symptoms+Suicide_Consider+Suicide_Attempt+Stress_High, data=HS_Pyramid_Report)
