# this script will make bar charts to compare the 5 regions
library(readxl)
library(ggplot2)
library(scales)

HS_Pyramid_Report <- read_excel("~/git/lab/comm_fairfax/data/comm_fairfax/original/2015 Supplemental Analysis by Pyramid Report__GIS.xlsx",
                                sheet = "8-10-12 Results by Pyramid")

#Making the new data table for the specific catergories
HS_Pyramid_Report1 <- HS_Pyramid_Report[,c(1,2,3,
                                           90,91,92,
                                           93,94,95,
                                           105,72,73,101,106,134,135,136,
                                           20,21,126,
                                           88,89,138,140,67,48,52,61)]
#accessing the youth survey data
#take some steps to clean up the data and name the columns
HS_Pyramid_Report1 <- HS_Pyramid_Report1[-1,]
colnames(HS_Pyramid_Report1) <- HS_Pyramid_Report1[1,]
HS_Pyramid_Report1 <- HS_Pyramid_Report1[-1,]

#we only select the columns most related to mental health issues
HS_Pyramid_Report1_mh <- HS_Pyramid_Report1[c('Pyramid_Number', 'Pyramid', 'Demographic', 'Depressive_Symptoms',
                                              'Suicide_Consider','Suicide_Attempt','Stress_Low','Stress_Medium','Stress_High',
                                              'Binge_Drinking','BulliedVic_School','BulliedVic_Not_School','Cigarette_30',
                                              'Marijuana_30','Physical_Activity_None','Physical_Activity_Daily',
                                              'Extracurricular_Available','Extracurricular_Regularly','Fruit_Veg_5','Cyberbullying_SchoolVictim',
                                              'Cyberbullying_SchoolAggressor','Sleep_4or less', 'Sleep_6','Parent_Help_Available','Adults_Talk','Gratitude','Food_Insecurity')]
#we only select the rows where they give us the overall score of the pyramid
HS_Pyramid_Report1_mh_overall <- subset(HS_Pyramid_Report1_mh, Demographic == "Overall")

#make all columns numerics
HS_Pyramid_Report1_mh_overall[,4:27] <- sapply(HS_Pyramid_Report1_mh_overall[4:27], as.numeric)
HS_Pyramid_Report1_mh_overall[,4:27] <- round(HS_Pyramid_Report1_mh_overall[,4:27], digits = 2)

Regions <- HS_Pyramid_Report1_mh_overall[1:5,]

#region 1
Regions[1,4:27] <- colMeans(subset(HS_Pyramid_Report1_mh_overall, Pyramid == 'HERNDON' | Pyramid == 'LANGLEY' | Pyramid == 'MADISON'| Pyramid == 'OAKTON'|
                          Pyramid == 'SOUTH LAKES')[4:27])
Regions[1,2] <- 'Region 1'

#region 2
Regions[2,4:27] <- colMeans(subset(HS_Pyramid_Report1_mh_overall, Pyramid == 'ANNANDALE' | Pyramid == 'FALLS CHURCH' | Pyramid == 'MCLEAN'| Pyramid == 'MARSHALL'|
                                       Pyramid == 'STUART' | Pyramid == 'THOMAS JEFFERSON')[4:27])
Regions[2,2] <- 'Region 2'

#region 3
Regions[3,4:27] <- colMeans(subset(HS_Pyramid_Report1_mh_overall, Pyramid == 'EDISON' | Pyramid == 'HAYFIELD' | Pyramid == 'LEE'| Pyramid == 'MOUNT VERNON'|
                                       Pyramid == 'WEST POTOMAC')[4:27])
Regions[3,2] <- 'Region 3'

#region 4
Regions[4,4:27] <- colMeans(subset(HS_Pyramid_Report1_mh_overall, Pyramid == 'CENTREVILLE' | Pyramid == 'LAKE BRADDOCK' | Pyramid == 'ROBINSON'| Pyramid == 'SOUTH COUNTY'|
                                       Pyramid == 'WEST SPRINGFIELD')[4:27])
Regions[4,2] <- 'Region 4'

#region 5
Regions[5,4:27] <- colMeans(subset(HS_Pyramid_Report1_mh_overall, Pyramid == 'CHANTILLY' | Pyramid == 'FAIRFAX' | Pyramid == 'WESTFIELD'| Pyramid == 'WOODSON')[4:27])
Regions[5,2] <- 'Region 5'

###########   start making the bar charts
#making title string to help identify region
s1 <- 'Herndon, Langley, Madison, \n Oakton'
s2 <-'Annandale, Falls Church, Mclean,\n Marshall, Stuart, Thomas Jefferson'
s3 <-'Edison, Hayfield, Lee, \n Mount Vernon, West Potomac'
s4 <-'Centreville, Lake Braddock, Robinson, \n South County, West Springfield'
s5 <-'Chantilly, Fairfax, Westfield, \n Woodson'
bchart <- ggplot(Regions, aes(x = reorder(Pyramid, -as.numeric(Depressive_Symptoms)),
                                               y= Depressive_Symptoms)) +
    geom_bar(stat = 'identity', aes(fill = as.numeric(Depressive_Symptoms))) +
    scale_fill_gradient2(low = '#19bd00', mid = '#f5f671', high = '#fd0000', midpoint = 26) +
    guides(fill = FALSE) +
    ggtitle("% of Overall Students reporting Depressive Symptoms across region")+
    labs(x= 'Region') +
    theme_bw()+
    theme(axis.text.x=element_text(size = 15), axis.text.y = element_text(size = 15)) +
    geom_text(aes(label=c(s1,s2,s3,s4,s5)), position=position_dodge(width=0.4), vjust=-0.25, size = 5) +
    scale_y_continuous(name="Percent", breaks = seq(20,32,1),limits=c(20, 32),oob = rescale_none)
plot(bchart)
#use this to save ggsave(both1, filename = "over_depress_with_bar.png",
       #path = "~/git/lab/comm_fairfax/data/comm_fairfax/working/Youth_Survey_Heat_Maps/with_bar_chart",
       #device = "png", width=20,height=11.25,scale=1)
