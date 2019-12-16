# Script for final set of glyph maps for poster. 

# install_github("garrettgman/ggsubplot")

library(ggmap)
library(tidyverse)
library(sp)
library(RColorBrewer)
library(grid)
library(gtable)
library(fiftystater)
library(reshape2)
library(mapproj)
# library(cowplot) # don't load explicitly as it changes default ggplot themes

# Project the 50 states and use the coordinates. 
data("fifty_states") # this line is optional due to lazy data loading

# Load summarized data set (by month)
weatherSum <- read.csv("../Data/summary_city_month.csv")
locations <- read.csv("../Data/clusterMapFinal.csv")

# Adjust the location of Alaska and Hawaii
longitude = c(-117.25, -108.50)
latitude = c(27.2, 27)
locations$latitude[locations$state == "Alaska"] = latitude[1]
locations$latitude[locations$state == "Hawaii"] = latitude[2]
locations$longitude[locations$state == "Alaska"] = longitude[1]
locations$longitude[locations$state == "Hawaii"] = longitude[2]

weatherSum$latitude[weatherSum$state == "Alaska"] = latitude[1]
weatherSum$latitude[weatherSum$state == "Hawaii"] = latitude[2]
weatherSum$longitude[weatherSum$state == "Alaska"] = longitude[1]
weatherSum$longitude[weatherSum$state == "Hawaii"] = longitude[2]

# Make a temporary data frame of coordinates so that everyting gets identically projected: 
coordDF <- data.frame(long = c(fifty_states$long, weatherSum$longitude, locations$longitude),
                      lat = c(fifty_states$lat, weatherSum$latitude, locations$latitude),
                      id = c(rep(1, nrow(fifty_states)), rep(2, nrow(weatherSum)), rep(3, nrow(locations))))

tcoords <- mapproject(coordDF$long, coordDF$lat, projection="mercator", 
                      parameters=NULL, orientation=NULL)

fifty_states$long <- tcoords$x[coordDF$id == 1]
fifty_states$lat <- tcoords$y[coordDF$id == 1]

weatherSum$longitude2 <- tcoords$x[coordDF$id == 2]
weatherSum$latitude2 <- tcoords$y[coordDF$id == 2]

locations$longitude2 <- tcoords$x[coordDF$id == 3]
locations$latitude2 <- tcoords$y[coordDF$id == 3]

weatherSum$dRad <- (pi/2) - ( ((2*pi)/12) * (weatherSum$month - 1) )

# Create a scaled version of the BSS ratio
weatherSum$sBSS <- 1-weatherSum$BSS
weatherSum$sBSS <- weatherSum$sBSS / max(weatherSum$sBSS)

weatherSum$Cluster <- locations$Cluster[match(weatherSum$AirPtCd, locations$AirPtCd)]

locations$stateABB <- "none"
for(i in 1:nrow(locations)){locations$stateABB[i] <- state.abb[grep(locations$state[i], state.name)]}
locations$state = tolower(locations$state)

# Use same color syntax for original map
pal2 <- brewer.pal(8, "Set1")[c(1:5, 8)]
pal2 <- pal2[c(4, 2, 3, 6, 1, 5)]

# Map 1 - small dots with colored circles to match clusters. 
#=============================================================================
map <- ggplot(locations, aes(map_id = state)) +
  # map points to the fifty_states shape data
  geom_map(fill = alpha("gray90", 0.5), color = alpha("gray80", 0.5), 
           map = fifty_states) +
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  geom_point(aes(x = longitude2, y = latitude2, 
                 color = factor(Cluster)), size = 0.1) + 
  geom_polygon(data = weatherSum, 
               aes(x = longitude2 + .025*sBSS*cos(dRad),
                   y = latitude2 + .025*sBSS*sin(dRad),
                   group = factor(AirPtCd), color = factor(Cluster)), 
               fill = NA) + 
  coord_fixed() +
  scale_color_manual(values = pal2) + 
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme(legend.position = "none",
        panel.background = element_blank())
#=============================================================================

# Map 2 - large dots with black circles. 
#=============================================================================
map2 <- ggplot(locations, aes(map_id = state)) +
  # map points to the fifty_states shape data
  geom_map(fill = alpha("gray90", 0.5), color = alpha("gray80", 0.5), 
           map = fifty_states) +
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  geom_point(aes(x = longitude2, y = latitude2, 
                 color = factor(Cluster)), size = 1) + 
  geom_polygon(data = weatherSum, 
               aes(x = longitude2 + .025*sBSS*cos(dRad),
                   y = latitude2 + .025*sBSS*sin(dRad),
                   group = factor(AirPtCd)), color = alpha("black", 0.8), 
               fill = NA) + 
  coord_fixed() +
  scale_color_manual(values = pal2) + 
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme(legend.position = "none",
        panel.background = element_blank())
#=============================================================================

# Map 3 - larger dots with black circles
#=============================================================================
map3 <- ggplot(locations, aes(map_id = state)) +
  # map points to the fifty_states shape data
  geom_map(fill = alpha("gray90", 0.5), color = alpha("gray80", 0.5), 
           map = fifty_states) +
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  geom_point(aes(x = longitude2, y = latitude2, 
                 color = factor(Cluster)), size = 2) + 
  geom_polygon(data = weatherSum, 
               aes(x = longitude2 + .025*sBSS*cos(dRad),
                   y = latitude2 + .025*sBSS*sin(dRad),
                   group = factor(AirPtCd)), color = alpha("black", 0.8), 
               fill = NA) + 
  coord_fixed() +
  scale_color_manual(values = pal2) + 
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme(legend.position = "none",
        panel.background = element_blank())
#=============================================================================

# Map 4 - larger dots with alpha values with black circles
#=============================================================================
map4 <- ggplot(locations, aes(map_id = state)) +
  # map points to the fifty_states shape data
  geom_map(fill = alpha("gray90", 0.5), color = alpha("gray80", 0.5), 
           map = fifty_states) +
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  geom_point(aes(x = longitude2, y = latitude2, 
                 color = factor(Cluster)), size = 2, alpha = 0.7) + 
  geom_polygon(data = weatherSum, 
               aes(x = longitude2 + .025*sBSS*cos(dRad),
                   y = latitude2 + .025*sBSS*sin(dRad),
                   group = factor(AirPtCd)), color = alpha("black", 0.7), 
               fill = NA) + 
  coord_fixed() +
  scale_color_manual(values = pal2) + 
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme(legend.position = "none",
        panel.background = element_blank())
#=============================================================================

pdf("../images/glyphPlots/glyphPrecipGallery.pdf", paper = "USr", width = 10, height = 8)
map
map2
map3
map4
dev.off()






# See coord_map() helpfile for suggested projections. 
# See following link for help with Albers equal area projections for the US.  
# http://desktop.arcgis.com/en/arcmap/latest/map/projections/albers-equal-area-conic.htm 


# # Distance bearing calculations:
# # http://www.geomidpoint.com/destination/calculation.html
# weatherSum.trig <- weatherSum %>%
#   mutate(distRad = 30/6372.7976,
#          latRad = latitude*pi/180,
#          lonRad = longitude*pi/180,
#          lat2 = asin(sin(latRad)*cos(distRad) + cos(latRad)*sin(distRad)*cos(dRad)),
#          lon2 = lonRad + atan2( sin(dRad)*sin(distRad)*cos(latRad), cos(distRad) - sin(latRad)*sin(lat2) ) 
#          )
# 
# weatherSum.trig <- weatherSum.trig %>%
#   mutate(dLon = lonRad - lon2,
#          y = sin(dLon)*cos(latRad),
#          x = cos(lat2)*sin(latRad) - sin(lat2)*cos(latRad)*cos(dLon),
#          d = atan2(y, x),
#          finalBrg = d + pi,
#          backBrg = d + 2*pi
#   )
# 
# weatherSum.trig <- weatherSum.trig %>%
#   mutate(lat2 = lat2*180/pi,
#          lon2 = lon2*180/pi,
#          finalBrg = finalBrg*180/pi,
#          backBrg = backBrg*180/pi
#   )




