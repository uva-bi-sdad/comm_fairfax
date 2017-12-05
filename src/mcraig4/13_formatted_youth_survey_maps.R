# I turned 2015 Supplemental Analysis by Pyramid Report__GIS into .csv
# Put them in the folder path data/comm_fairfax/working/formatted_youth_survey/

# Copied over GIS shapefiles from limbo workspace into:
# data/comm_fairfax/working/formatted_youth_survey/

library(rgdal)

sixth <- read.csv('data/comm_fairfax/working/formatted_youth_survey/2015_supplemental_6_pyramid.csv')
older <- read.csv('data/comm_fairfax/working/formatted_youth_survey/2015_supplemental_8_10_12_pyramid.csv')

pyramids <- readOGR('data/comm_fairfax/working/formatted_youth_survey/High_School_Pyramids',
                    "High_School_Attendance_Areas")

# Look back at Eirik's code to join the .csv with the shapefile
# Name of HS is in pyramids
# How do you get this shapefile into working map_data?
