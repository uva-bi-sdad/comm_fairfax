library(readxl)
library(ggplot2)

SDV<-read_excel("~/Documents/Fairfax/Physical Activity/Data /Supervisor District Variables.xls") 

SDV$District<-ordered(SDV$District,levels=c("SPRINGFIELD","DRANESVILLE","SULLY","HUNTER MILL",
                                            "MOUNT VERNON","BRADDOCK","PROVIDENCE","LEE","MASON"),
        labels=c("Springfield","Dranesville","Sully","Hunter Mill","Mount Vernon",
                 "Braddock","Providence","Lee","Mason"))


#Color Blind Palettes
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
wheel <- function(col, radius = 1, ...)
  pie(rep(1, length(col)), col = col, radius = radius, ...)
wheel(cbPalette)
 
SDV.noEVI<-as.data.frame(SDV[c(10:dim(SDV)[1]),])
variable_names<-c(
  "HB"="Housing Burden >50%",
  "NoHI"="No Health Insurance",
  "NV"="No Vehicle", 
  "Poverty"="Poverty",
  "SNAP"="SNAP",
  "UnEmp"="Unemployed"
)

Varmn<-aggregate(list(Percent=SDV.noEVI$Percent),list(Variable=SDV.noEVI$Variable), mean)

par(mgp=c(3,0,0))
p<-ggplot(SDV.noEVI, aes(x=District, y=Percent)) +
  geom_segment(aes(x=District, xend=District, y=0, yend=Percent), color=cbPalette[1]) +
  geom_point(shape=21, colour="black", fill=cbPalette[7], size=4) +
  coord_flip() +
  theme_bw() +
  facet_wrap(~Variable, scales="free", labeller=as_labeller(variable_names)) +
  geom_hline(data=Varmn, aes(yintercept=Percent), colour="black", linetype="longdash") +
  geom_point(shape=21, colour="black", fill=cbPalette[7], size=4) +
  theme(strip.text=element_text(size=16, face="bold"),
        strip.background=element_rect(fill=cbPalette[5]),
        axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15)) +
  xlab("") + 
  ylab("") 

pdf("Supervisor District Lollipop Plot.pdf",width=12,height=6)
p
dev.off()

##########################################################################################
SDPR<-read_excel("~/Documents/Fairfax/Physical Activity/Data /SD_PR.xls") 

SDPR$District<-ordered(SDPR$District,levels=c("SPRINGFIELD","DRANESVILLE","SULLY","HUNTER MILL",
                                            "MOUNT VERNON","BRADDOCK","PROVIDENCE","LEE","MASON"),
                      labels=c("Springfield","Dranesville","Sully","Hunter Mill","Mount Vernon",
                               "Braddock","Providence","Lee","Mason"))


#Color Blind Palettes
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
wheel <- function(col, radius = 1, ...)
  pie(rep(1, length(col)), col = col, radius = radius, ...)
wheel(cbPalette)

variable_names<-c(
  "RRPC"="Registration Rate/Capita",
  "MRPC"="Membership Rate/Capita",
  "SRPR"="Scholarship Rate/Registrant", 
  "SFOTC"="Scholarship Fraction of Total Cost"
)

Varmn<-aggregate(list(Percent=SDPR$Percent),list(Variable=SDPR$Variable), mean)

par(mgp=c(3,0,0), mar=c(5.1,4.1,4.1,5.1))
p<-ggplot(SDPR, aes(x=District, y=Percent)) +
  geom_segment(aes(x=District, xend=District, y=0, yend=Percent), color=cbPalette[1]) +
  geom_point(shape=21, colour="black", fill=cbPalette[7], size=4) +
  coord_flip() +
  theme_bw() +
  facet_wrap(~Variable, scales="free", labeller=as_labeller(variable_names)) +
  geom_hline(data=Varmn, aes(yintercept=Percent), colour="black", linetype="longdash") +
  geom_point(shape=21, colour="black", fill=cbPalette[7], size=4) +
  theme(strip.text=element_text(size=16, face="bold"),
        strip.background=element_rect(fill=cbPalette[5]),
        axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15)) +
  xlab("") + 
  ylab("") 

pdf("Supervisor District PR Lollipop Plot.pdf",width=12,height=6)
p
dev.off()


