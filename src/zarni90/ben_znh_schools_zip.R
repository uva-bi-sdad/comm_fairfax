### Plots by zip code fairfax county - ACS data
library(rgdal)
library(maptools)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(reshape2)
library(scales)
#library(OpenStreetMap)
library(osmar)
library(ggmap)
library(raster)
library(rasterVis)
library(data.table)
library(RColorBrewer)
library(sp)


##LOAD 17-27
## From the internet to make Raster Maps from Lat Long Rectangle
## @BEN: Fill in the source here

mgmap <- get_map(location=c(-77.7173, 38.5976, -76.8686, 39.0682), source = "google", color = "bw")
vgmap <- as.vector(mgmap)
vgmaprgb <- col2rgb(vgmap)
gmapr <- matrix(vgmaprgb[1, ], ncol = ncol(mgmap), nrow = nrow(mgmap))
gmapg <- matrix(vgmaprgb[2, ], ncol = ncol(mgmap), nrow = nrow(mgmap))
gmapb <- matrix(vgmaprgb[3, ], ncol = ncol(mgmap), nrow = nrow(mgmap))
rgmaprgb <- brick(raster(gmapr), raster(gmapg), raster(gmapb))
class(rgmaprgb)
rgmaprgb
rm(gmapr, gmapg, gmapb)
#Here, we are the CRS for the empty raster from above.
projection(rgmaprgb) <- CRS("+init=epsg:4326")
proj4string(rgmaprgb) #Checking the Projection String

##MAY NOT NEED THE CODE BELOW
extent(rgmaprgb) <- unlist(attr(mgmap, which = "bb"))[c(2, 4, 1, 3)]
unlist(attr(mgmap, which = "bb")[c(2,4,1,3)])
attr(mgmap, which = "bb")
rgmaprgb
##MAY NOT NEED THE CODE ABOVE


### get map from openstreetmap
#mp <- openmap(c(39.0682,-77.7173),c(38.5976,-76.8686),type='osm')



#mp<-get_map(location=c(-77.7173,38.5976,-76.8686,39.0682),source="google")

# GIS data
##Set to this working directory OR move data to
setwd("~/sdal/projects/limbo/fairfax_alerts/")
county <- readShapePoly("GISData/Fairfax_County_Border/Fairfax_County_Border.shp",
                        proj4string=CRS('+proj=longlat +ellps=WGS84'))
zip <- readShapePoly("GISData/ZIP_Codes/ZIP_Codes.shp",
                     proj4string=CRS('+proj=longlat +ellps=WGS84'))
names(zip@data)
length(zip@data$ZIPCODE)

#Give polygon GIS data to highSchool
highSchool <- readShapePoly("GISData/High_School_Pyramids/High_School_Attendance_Areas.shp",
                            proj4string=CRS('+proj=longlat +ellps=WGS84'))

county <- spTransform(county,rgmaprgb@crs)
zip <- spTransform(zip,rgmaprgb@crs)

#NEED TO LOAD THIS LINE
#Transforming to the same CRS system and make it Spatial Points
#In order to be plottable
highSchool <- spTransform(highSchool, rgmaprgb@crs)
colnames(highSchool@data)
test <-(fortify(highSchool))

test$group[1:10]

####STOP HERE AND YOU CAN DO: plot(highSchool)

#OVERLAY PLOT @ZNH
#It divides the plot area by (row x column) however many u want
par(mfrow=c(1,1))
plot(highSchool, col = "red", main = "High School Boundary (red) \n vs Zipcode (green)")
lines(zip, col = "green")


#LOADING MENTAL HEALTH PROVIDERS
mhp_clean <- rio::import("~/git/comm_fairfax/data/comm_fairfax/original/mhp_clean.csv")

#The spatial points is for putting in the mental health providers into the map
long_lat_mhp <- SpatialPoints(cbind(Long=as.numeric(mhp_clean$longitude), Lat=as.numeric(mhp_clean$latitude)))
#The spatial data frame is for the Heat
long_lat_mhp_frame <- SpatialPointsDataFrame(cbind(lon = as.numeric(mhp_clean$longitude), lat = as.numeric(mhp_clean$latitude)),data = mhp_clean)
#On the same coordinate reference system as highSchool/or your plot Map
proj4string(long_lat_mhp_frame) <- proj4string(highSchool)


#crime$lon
mhp_clean$longitude

length(long_lat_mhp)
#long_lat_mhp@coords[,1]

#CREATE COORDINATE REFERENCE SYSTEM
crs.geo <- CRS("+init=epsg:4326")
proj4string(long_lat_mhp) <- crs.geo

#OVERLAY & COUNT
nrow(highSchool@data)
#Gives Associated Polygon for each Spatial Point
#
sch_with_mhp <- as.data.table(over(long_lat_mhp,highSchool))
names(sch_with_mhp)
head(sch_with_mhp)
mhp_per_sch <- sch_with_mhp[,.N,OBJECTID]
mhp_per_sch
#Why are we joining here?
#Joining to get additonal high school information
mhp_per_sch <- mhp_per_sch %>% left_join(highSchool@data, by = c("OBJECTID", "OBJECTID"))
mhp_per_sch[1:3,]
nrow(mhp_per_sch)
#Have all the high schools preserved.
highSchool_count <- highSchool@data %>% left_join(mhp_per_sch, by = c("OBJECTID","OBJECTID"))
nrow(highSchool_count)


length(mhp_per_sch$OBJECTID)

#CHECK COMMENTS HERE!
#This converts the polygon object to a dataframe that includes all the points that make the 25 high school polygons
highSchool_count <- fortify(highSchool)

names(highSchool_count)
nrow(highSchool_count)


#IN CENSUS BLOCK: YOU WILL HAVE MORE UNIQUE IDS:
#25 unique objects here
length(unique(highSchool_count$id))

nrow(mhp_per_sch)
View(mhp_per_sch)
#Histogram in GGPLOT
#Things to modify
#1 Make the counts disecrete
#2 Title the Graph
#3 Label the X-Axis
head(mhp_per_sch)
ggplot(data = mhp_per_sch, aes (x=SCHOOL_NAM, y = N, fill = N)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle=90, hjust =1, vjust=0.5, size = 7.5), panel.grid.major = element_blank()) + ggtitle("Count of Mental Health Providers by High School Pyramid") +
    labs(y= "Number of Mental Health Providers", x = "High School Name") +
    scale_y_discrete(limits=1:10, labels = 1:10) +
    scale_fill_gradient2(low = '#d8b365', mid = '#808080', high = '#5ab4ac', midpoint = 5)


#d8b365
#f5f5f5
#5ab4ac
#Plotting the High School Map

plot(highSchool, asp = 1, main = "Mental Health Providers \n within High School Boundary")
points(mhp_clean$longitude, mhp_clean$latitude, pty = 16, cex = 0.25, col = "red")
#Sb will figure out what the hell is wrong with pty and cex above?

#WANT A HEAT MAP (IGNORES THE SCHOOL THAT HAS ZERO ASSIGN)

#This is Zip Polygon of all Fairfax County Zipcodes
#IGNORE ALL THIS BELOW. IT"S IN MARKDOWN
#Data of the zip polygon file
names(zip@data)
zip@polygons
zip@data$id = rownames(zip@data)
nrow(zip@data)
#this breaksdown the zip polygon into a data frame. This code is from Bianica
zip.points = fortify(zip, region="id")
names(zip.points)

zip.df2 = left_join(zip.points, zip@data, by="id")


#For next steps may need to convert highSchool GIS data into datafram
highSchool@data$id = rownames(highSchool@data)
highSchool.points = fortify(highSchool, region="id")
highSchool.df2 = left_join(highSchool.points, highSchool@data, by="id")





# import acs data
acs<-read.csv("ACSData/ACSData_Fairfax_Zipcodesv2.csv")

# map households that are 65+ year old living alone
df<-dplyr::select(acs,zipcode,totalPopulation,totalHH,totalPopulation5YearsUp,hhW65PlusLivingAlone,noIns,popBelowPoverty,
                  ownerHHNoVeh,renterHHNoVeh,popForeignBornEntered2010OrLater,popWDisability,popSpeaksEngNotWell,popSpeaksEngNotAtAll,
                  familySingleParentMale,familySingleParentFemale,nonFamilySingleMale,nonFamilySingleFemale) %>%
    mutate(hhNoVehicle=ownerHHNoVeh+renterHHNoVeh,
           popSpeaksEngNotVeryWellNotAtAll=popSpeaksEngNotWell+popSpeaksEngNotAtAll,
           singleHeadOfHH=familySingleParentMale+familySingleParentFemale+nonFamilySingleMale+nonFamilySingleFemale,
           perHH65LivingAlone=hhW65PlusLivingAlone/totalHH,
           perHHSingleParents=singleHeadOfHH/totalHH,
           perPopNoIns=noIns/totalPopulation,
           perPopBelowPov=popBelowPoverty/totalPopulation,
           perHHNoVehicle=hhNoVehicle/totalHH,
           perPopForeignBornEntered2010OrLater=popForeignBornEntered2010OrLater/totalPopulation,
           perPopWDisability=popWDisability/totalPopulation,
           perPopSpeaksEngNotVeryWellNotAtAll=popSpeaksEngNotVeryWellNotAtAll/totalPopulation5YearsUp
    )
df<-df[c(1,21:28)]
df<-melt(df,id.var=c("zipcode"))
df<-left_join(zip.df2,df,by=c("ZIPCODE"="zipcode"))

# some ggplot of this
num<-length(unique(df$variable))
vars<-unique(df$variable)
titles<-c("Households 65+ Living Alone","Single Parent Households with Children Under 18", "Population with No Medical Insurance","Population Below Povery",
          "Housheolds with No Vehicle", "Population Foreign Born that Entered 2010 or Later", "Population with a Disability", "Population that does not Speak English Well or Very Well")

setwd("~/sdal/projects/limbo/fairfax_alerts/Rstuff/plotsBianica")
for (i in 1: num) {
    plot<-filter(df,variable==vars[i])
    #png(paste0(vars[i],'.png'),width=1089,height=1104.5)
    pdf(paste0(vars[i],'.pdf'),width=8,height=6)

    gplot(rgmaprgb,expand=F) +
        geom_polygon(aes(long,lat,group=group,fill=value),
                     data=plot, alpha=.7,color="grey70",lwd=.5) +
        scale_fill_gradient(low="white",high="red",labels=percent,name=expression("Percent of \nPopulation in \nZipcode")) +
        coord_equal(ratio=1) +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
              axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
              plot.title = element_text(lineheight=.8, face="bold", vjust=1, hjust = .5),
              plot.caption = element_text(hjust=0)) + #labels
        labs(title=titles[i], x="", y="", caption="Source: US American Community Survey data for Fairfax County, VA, 2015")

    dev.off()
}

#t<-ggplot_build(p)$data
#unique(t[[1]][[1]]) ## get list of colors used
#?scale_colour_gradient


### Charities
charities<-read.csv("~/sdal/projects/limbo/fairfax_alerts/Data/charRegZipFairfax.csv")
charities<-left_join(charities,zip.df2,by=c("Zip.Code"="ZIPCODE"))
pdf('charities.pdf',width=8,height=6)
autoplot(mp,expand=F) +
    geom_polygon(aes(long,lat,group=group,fill=Ncharity),
                 data=charities, alpha=.7,color="grey70",lwd=.5) +
    scale_fill_gradient(low="white",high="red",name=expression("Number of \nNon-Profits per \nZipcode")) +
    coord_equal(ratio=1) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
          axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
          plot.title = element_text(lineheight=.8, face="bold", vjust=1, hjust = .5),
          plot.caption = element_text(hjust=0)) + #labels
    labs(title="Non-Profits", x="", y="", caption="Source: Aunt Bertha Connecting People and Programs (https://www.auntbertha.com/)")

dev.off()

reg<-left_join(charities,dplyr::select(acs,zipcode,totalPopulation),by=c("Zip.Code"="zipcode")) %>%
    mutate(perPopReg=reg/totalPopulation)
pdf('registrations.pdf',width=8,height=6)
autoplot(mp,expand=F) +
    geom_polygon(aes(long,lat,group=group,fill=perPopReg),
                 data=reg, alpha=.7,color="grey70",lwd=.5) +
    scale_fill_gradient(low="white",high="red",labels=percent,name=expression("Percent of \nPopulation in \nZipcode")) +
    coord_equal(ratio=1) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
          axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
          plot.title = element_text(lineheight=.8, face="bold", vjust=1, hjust = .5),
          plot.caption = element_text(hjust=0)) + #labels
    labs(title="Registrations", x="", y="", caption="Source: Office of Emergency Management - Fairfax County, VA")
dev.off()

pdf('registrations_count.pdf',width=8,height=6)
autoplot(mp,expand=F) +
    geom_polygon(aes(long,lat,group=group,fill=reg),
                 data=reg, alpha=.7,color="grey70",lwd=.5) +
    scale_fill_gradient(low="white",high="red",name=expression("Number of \nRegistrations per \nZipcode")) +
    coord_equal(ratio=1) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
          axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
          plot.title = element_text(lineheight=.8, face="bold", vjust=1, hjust = .5),
          plot.caption = element_text(hjust=0)) + #labels
    labs(title="Registrations", x="", y="", caption="Source: Office of Emergency Management - Fairfax County, VA")
dev.off()
