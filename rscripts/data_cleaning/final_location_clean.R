# Final location cleaning script. Consolidates all location information into 
# one common script and saves two versions. One with ALL considered information
# and another with only the information used in the current version of the plots. 
# Created: 6-22-2018
# Last Updated: 8-31-2018

# Script to get elevation data for each airport location 
# (and many many others)

# load weatherData package (installed from tar.gz file in data folder. )
library(weatherData)
library(dplyr) # for joins
library(rgbif) # for elevation queries
library(tidyr) # for separate function
library(RColorBrewer) # for cluster colors
library(proj4) # for coordinate manipulation

# Read in locations data. 
locations <- read.csv("../../data/locations.csv")

# Step 1 - add elevation figures
#=============================================================================

# Note that this method no longer works. It has been left in to show what we 
# did for the presenation and paper. 

# Obtain elevation values for locations from Google
# https://developers.google.com/maps/documentation/elevation/intro
# Use google server api key
# apikey <- NA # use your own api
# 
# locations$elevation <- -9999
# for(i in 1:nrow(locations)){
#   locations$elevation[i] <- elevation(latitude = locations$latitude[i],
#                                       longitude = locations$longitude[i],
#                                       key = apikey)$elevation
# }

# Add in the elevation from data we already acquired using the above method. 

elevation <- read.csv("../../data/elevation.csv")
locations$elevation <- elevation$elevation[match(locations$AirPtCd, elevation$AirPtCd)]

#=============================================================================

# Step 2 - Merge with original weather data station files. 
# (And clean up mispelled city names)
#=============================================================================
# Now read in the airport data (obtained from old weatherData package)
data("USAirportWeatherStations")

# Rename the third column of "airports" to match locations file. 
colnames(USAirportWeatherStations)[3] <- "AirPtCd"

# create state.temp to match format of census
# Harve MT should be Havre, MT
locations2 <- left_join(locations, 
                        USAirportWeatherStations, by = "AirPtCd") %>%
  mutate(lon.arpt = Lon,
         lat.arpt = Lat,
         elevation = round(elevation),
         elev.arpt = Elevation,
         state.arpt = State,
         name.arpt = Station,
         city = gsub(city, pattern = "[[:punct:]]", replacement = ""),
         city = gsub(city, pattern = "Harve", replacement = "Havre")) %>% 
  select(city, state,longitude, 
         latitude, elevation, AirPtCd, name.arpt, state.arpt, 
         lon.arpt, lat.arpt, elev.arpt, WMO)

# state is only different for one location: portland oregon 
# and vancouver washington, which are bordering cities. 
#=============================================================================

# Step 3 - Add population information
#=============================================================================
# Now add the populations of the major cities of interest 
# (see if that is a factor.)
# Found at: 
# - https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?src=bkmk
census <- read.csv("../../data/censusFigs.csv")
census <- dplyr::select(census, Geography, 
                        April.1..2010...Census, 
                        April.1..2010...Estimates.Base, 
                        Population.Estimate..as.of.July.1....2016)
names(census) <- c("Location", "census2010", "est2010", "est2016")

# Split city, state specification
census <- census %>% 
  separate(col = Location, into = c("city", "state"), sep = ",") %>%
  mutate(city = gsub(city, pattern = " [[:alpha:]]+$", replacement = ""),
         city = gsub(city, pattern = "/.+$", replacement = ""),
         city = gsub(city, pattern = "[[:punct:]]", replacement = ""),
         city = gsub(city, pattern = " city (balance)", replacement = ""),
         state = gsub(state, pattern = "^[[:space:]]", replacement = ""))

# Some final manual adjustments for weird mismatches
census <- census %>%
  mutate(city = gsub(city, pattern = "Boise City", 
                     replacement = "Boise"),
         city = gsub(city, pattern = "Baker City", 
                     replacement = "Baker"),
         city = gsub(city, pattern = "Urban Honolulu", 
                     replacement = "Honolulu"),
         city = gsub(city, pattern = "NashvilleDavidson metropolitan government balance", 
                     replacement = "Nashville"))

locations3 <- left_join(locations2, census, by = c("city", "state"))
check <- locations3 %>% filter(is.na(est2016))

# Only one location wasn't in the Census files, information from 
# this location is found manually at 
# - https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?src=bkmk
# I put the 2010 census value as the value for ALL estimates 
# to avoid missing values
locations3[locations$city == "Austin" & locations$state == "Nevada", 13:15] <-
  c(192, 192, 192)
#=============================================================================

# Step 4 - add coastline information
#=============================================================================
library(sp) # for spatial plots
library(rgdal) # for reading shapefiles
library(fields) # for distance calculations
library(maptools)
library(geosphere) # for point to line distance calculations

# Read in shapefile for coast (this includes the great lakes and major
# estuaries, but not alaska and Hawaii)
locs <- locations3

coast <- readOGR(dsn = "../../data/us_medium_shoreline",
                     layer = "us_medium_shoreline")

coastAK <- readOGR(dsn = "../../data/shorelineAK", layer = "CUSPLine")

coastHW <- readOGR(dsn = "../../data/shorelineHA", layer = "CUSPLine")

coastCoord <- coordinates(coast)
coord.vec <- lapply(coastCoord, as.data.frame)
# Assign coastal ID variable to each line vector.
for(i in 1:length(coord.vec)) {coord.vec[[i]]$ID <- i}
coord.table <- data.table::rbindlist(coord.vec)

locs.dist <- rdist.earth(x1 = locs[, 3:4], x2 = coord.table[, 1:2], miles = FALSE)

locs.min <- apply(locs.dist, 1, min)
locs.whichMin <- apply(locs.dist, 1, which.min)

locs$dist2coast <- locs.min
locs$coastRegion <- coast@data$REGIONS[coord.table$ID[locs.whichMin]]

# Redo the results for Alaska and Hawaii
proj4string(coastAK) <- CRS("+proj=longlat")
temp.ak <- dist2Line(p = locs[112, 3:4], line = coastAK)

proj4string(coastHW) <- CRS("+proj=longlat")
temp.hw <- dist2Line(p = locs[113, 3:4], line = coastHW)

locs$dist2coast[112] <- temp.ak[1] / 1000
locs$dist2coast[113] <- temp.hw[1] / 1000

write.csv(locs, file = "../../data/locationsFinal_preclust.csv", row.names = FALSE)


