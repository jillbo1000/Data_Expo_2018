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
locations$Type <- "Precipitation"
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
  facet_grid(~Type) + 
  coord_fixed() +
  scale_color_manual(values = pal2) + 
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  theme(legend.position = "none",
        panel.background = element_rect(fill = NA, color = "gray"),
        axis.text = element_blank(),
        axis.title = element_blank(),
        strip.text = element_text(size = 18))
#=============================================================================

pdf("../images/glyphPlots/glyphPrecip.pdf", paper = "USr", width = 20, height = 16)
map
dev.off()


png("../images/glyphPlots/glyphPrecip.png", width = 14, height = 10, units = "in", res = 300)
map
dev.off()



