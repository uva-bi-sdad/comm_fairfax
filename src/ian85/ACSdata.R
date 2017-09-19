library(acs)
library(rgdal)    # for readOGR and others
library(sp)       # for spatial objects
library(leaflet)  # for interactive maps (NOT leafletR here)
library(dplyr)    # for working with data frames
library(ggplot2)
library(mapproj)
library(tigris)
library(stringr)  # to pad fips codes
api.key.install(key = "a1957eda1f3d9e48c653a0a423bcd64967efced2")
options(tigris_use_cache = TRUE)
cali = geo.make(state = "CA", county = "*", tract = "*")

# Extract FIPS codes for CA regions
counties = as.numeric(substr(county.fips[grep("california", county.fips$polyname), 1], 3, 4))

tracts = tracts(state = "CA", cb = T)

acs.data = acs.fetch(endyear = 2015, span = 5, geography = cali, table.number = "B01001", col.names = "pretty")

acs.clean <- data.frame(paste0(str_pad(acs.data@geography$state, 2, "left", pad="0"),
                               str_pad(acs.data@geography$county, 3, "left", pad="0"),
                               str_pad(acs.data@geography$tract, 6, "left", pad="0")),
                        acs.data@estimate, stringsAsFactors = FALSE)

acs.clean = select(acs.clean, c(1:3, 27))
names(acs.clean) = c("GEOID", "Total Pop", "Male Pop", "Female Pop")

acs.merge = geo_join(tracts, acs.clean, "GEOID", "GEOID")

# Make a happy little map

popup <- paste0("GEOID: ", income_merged$GEOID, "<br>", "Percent of Households above $200k: ", round(income_merged$percent,2))
pal <- colorNumeric(
    palette = "YlGnBu",
    domain = income_merged$percent
)

map3<-leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(data = income_merged,
                fillColor = ~pal(percent),
                color = "#b2aeae", # you need to use hex colors
                fillOpacity = 0.7,
                weight = 1,
                smoothFactor = 0.2,
                popup = popup) %>%
    addLegend(pal = pal,
              values = income_merged$percent,
              position = "bottomright",
              title = "Percent of Households<br>above $200k",
              labFormat = labelFormat(suffix = "%"))
map3
