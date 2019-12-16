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

weatherSum$Cluster <- locations$Cluster[match(weatherSum$AirPtCd, locations$AirPtCd)]

weatherSum.melt <- reshape2::melt(weatherSum, measure.vars = c("mxT_mean_abs", "mnT_mean_abs"))

# Scale the error values to the maximum of the combined set.
maxEr <- max(weatherSum.melt$value)
minEr <- min(weatherSum.melt$value)
weatherSum.melt$value2 <- weatherSum.melt$value / maxEr

weatherSum.melt$group <- paste(weatherSum.melt$AirPtCd, weatherSum.melt$variable, sep = "")

locations$stateABB <- "none"
for(i in 1:nrow(locations)){locations$stateABB[i] <- state.abb[grep(locations$state[i], state.name)]}
locations$state = tolower(locations$state)

# Use same color syntax for original map
pal2 <- brewer.pal(8, "Set1")[c(1:5, 8)]
pal2 <- pal2[c(4, 2, 3, 6, 1, 5)]

# Map 1 - small dots/black circles
#=============================================================================
# guide = FALSE idea:
# - https://stackoverflow.com/questions/14604435/turning-off-some-legends-in-a-ggplot/14604540
map <- ggplot(locations, aes(map_id = state)) +
  # map points to the fifty_states shape data
  geom_map(fill = alpha("gray90", 0.5), color = alpha("gray80", 0.5), 
           map = fifty_states) +
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  geom_point(aes(x = longitude2, y = latitude2, 
                 color = factor(Cluster)), size = 0.1) + 
  geom_polygon(data = weatherSum.melt, 
               aes(x = longitude2 + .02*value2*cos(dRad),
                   y = latitude2 + .02*value2*sin(dRad),
                   group = factor(group),
                   linetype = factor(variable)), 
               fill = NA, col = "black") + 
  coord_fixed() +
  scale_color_manual(values =  pal2, guide = FALSE) + 
  scale_linetype_manual("Absolute Error", values = c(1, 2),
                        labels = c("Max Temp", "Min Temp")) + 
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme(legend.position = c(.6, .1),
        panel.background = element_blank())
#=============================================================================

# Map 2 - small dots/colored circles
#=============================================================================
# guide = FALSE idea:
# - https://stackoverflow.com/questions/14604435/turning-off-some-legends-in-a-ggplot/14604540
map2 <- ggplot(locations, aes(map_id = state)) +
  # map points to the fifty_states shape data
  geom_map(fill = alpha("gray90", 0.5), color = alpha("gray80", 0.5), 
           map = fifty_states) +
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  geom_point(aes(x = longitude2, y = latitude2, 
                 color = factor(Cluster)), size = 0.1) + 
  geom_polygon(data = weatherSum.melt, 
               aes(x = longitude2 + .02*value2*cos(dRad),
                   y = latitude2 + .02*value2*sin(dRad),
                   group = factor(group),
                   linetype = factor(variable), color = factor(Cluster)), 
               fill = NA) + 
  coord_fixed() +
  scale_color_manual(values =  pal2, guide = FALSE) + 
  scale_linetype_manual("Absolute Error", values = c(1, 2),
                        labels = c("Max Temp", "Min Temp")) + 
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme(legend.position = "none",
        panel.background = element_blank())
#=============================================================================

# Map 3 - larger dots/black circles
#=============================================================================
# guide = FALSE idea:
# - https://stackoverflow.com/questions/14604435/turning-off-some-legends-in-a-ggplot/14604540
map3 <- ggplot(locations, aes(map_id = state)) +
  # map points to the fifty_states shape data
  geom_map(fill = alpha("gray90", 0.5), color = alpha("gray80", 0.5), 
           map = fifty_states) +
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  geom_point(aes(x = longitude2, y = latitude2, 
                 color = factor(Cluster)), size = 1) + 
  geom_polygon(data = weatherSum.melt, 
               aes(x = longitude2 + .02*value2*cos(dRad),
                   y = latitude2 + .02*value2*sin(dRad),
                   group = factor(group),
                   linetype = factor(variable)),
               color = alpha("black", 0.7), 
               fill = NA) + 
  coord_fixed() +
  scale_color_manual(values =  pal2, guide = FALSE) + 
  scale_linetype_manual("Absolute Error", values = c(1, 2),
                        labels = c("Max Temp", "Min Temp")) + 
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme(legend.position = "none",
        panel.background = element_blank())
#=============================================================================

# Map 4 - even larger dots with black circles
#=============================================================================
# guide = FALSE idea:
# - https://stackoverflow.com/questions/14604435/turning-off-some-legends-in-a-ggplot/14604540
map4 <- ggplot(locations, aes(map_id = state)) +
  # map points to the fifty_states shape data
  geom_map(fill = alpha("gray90", 0.5), color = alpha("gray80", 0.5), 
           map = fifty_states) +
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  geom_point(aes(x = longitude2, y = latitude2, 
                 color = factor(Cluster)), size = 2, alpha = 0.7) + 
  geom_polygon(data = weatherSum.melt, 
               aes(x = longitude2 + .02*value2*cos(dRad),
                   y = latitude2 + .02*value2*sin(dRad),
                   group = factor(group),
                   linetype = factor(variable)),
               color = alpha("black", 0.7), 
               fill = NA) + 
  coord_fixed() +
  scale_color_manual(values =  pal2, guide = FALSE) + 
  scale_linetype_manual("Absolute Error", values = c(1, 3),
                        labels = c("Max Temp", "Min Temp")) + 
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme(legend.position = "none",
        panel.background = element_blank())
#=============================================================================

# Map 5 - even larger dots with black/gray circles (no linetypes)
#=============================================================================
# guide = FALSE idea:
# - https://stackoverflow.com/questions/14604435/turning-off-some-legends-in-a-ggplot/14604540
map5 <- ggplot(locations, aes(map_id = state)) +
  # map points to the fifty_states shape data
  geom_map(fill = alpha("gray90", 0.5), color = alpha("gray80", 0.5), 
           map = fifty_states) +
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  geom_point(aes(x = longitude2, y = latitude2, 
                 color = factor(Cluster)), size = 2, alpha = 0.7) + 
  geom_polygon(data = weatherSum.melt, 
               aes(x = longitude2 + .02*value2*cos(dRad),
                   y = latitude2 + .02*value2*sin(dRad),
                   group = factor(group)),
               color = c(alpha("black", 0.7), alpha("gray40", 0.8))[as.numeric(factor(weatherSum.melt$variable))], 
               fill = NA) + 
  coord_fixed() +
  scale_color_manual(values =  pal2, guide = FALSE) + 
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme(legend.position = "none",
        panel.background = element_blank())
#=============================================================================

# Map 6 - even larger dots with LARGER black/gray circles (no linetypes)
#=============================================================================
# guide = FALSE idea:
# - https://stackoverflow.com/questions/14604435/turning-off-some-legends-in-a-ggplot/14604540
map6 <- ggplot(locations, aes(map_id = state)) +
  # map points to the fifty_states shape data
  geom_map(fill = alpha("gray90", 0.5), color = alpha("gray80", 0.5), 
           map = fifty_states) +
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  geom_point(aes(x = longitude2, y = latitude2, 
                 color = factor(Cluster)), size = 2, alpha = 0.7) + 
  geom_polygon(data = weatherSum.melt, 
               aes(x = longitude2 + .025*value2*cos(dRad),
                   y = latitude2 + .025*value2*sin(dRad),
                   group = factor(group)),
               color = c(alpha("black", 0.7), alpha("gray40", 0.8))[as.numeric(factor(weatherSum.melt$variable))], 
               fill = NA) + 
  coord_fixed() +
  scale_color_manual(values =  pal2, guide = FALSE) + 
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme(legend.position = "none",
        panel.background = element_blank())
#=============================================================================

# Map 7 - even larger dots with LARGER black/gray circles (no linetypes) with non-circle points. 
#=============================================================================
# guide = FALSE idea:
# - https://stackoverflow.com/questions/14604435/turning-off-some-legends-in-a-ggplot/14604540
map7 <- ggplot(locations, aes(map_id = state)) +
  # map points to the fifty_states shape data
  geom_map(fill = alpha("gray90", 0.5), color = alpha("gray80", 0.5), 
           map = fifty_states) +
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  geom_point(aes(x = longitude2, y = latitude2, 
                 color = factor(Cluster)), size = 3, alpha = 0.7, pch = "+") + 
  geom_polygon(data = weatherSum.melt, 
               aes(x = longitude2 + .025*value2*cos(dRad),
                   y = latitude2 + .025*value2*sin(dRad),
                   group = factor(group)),
               color = c(alpha("black", 0.7), alpha("gray40", 0.8))[as.numeric(factor(weatherSum.melt$variable))], 
               fill = NA) + 
  coord_fixed() +
  scale_color_manual(values =  pal2, guide = FALSE) + 
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme(legend.position = "none",
        panel.background = element_blank())
#=============================================================================

# Map 8 - small dots, different line thickness/intensity for each cluster. 
#=============================================================================
set1 <- c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#f781bf")
set1 <- set1[c(4, 2, 3, 6, 1, 5)] # Reorder to match the original layout
set2 <- c("#fb9a99", "#a6cee3", "#b2df8a", "#cab2d6", "#fdbf6f", "#fccde5")
set2 <- set2[c(4, 2, 3, 6, 1, 5)] # Reorder to match the original layout

weatherSum.melt$col <- "black"
weatherSum.melt$col[weatherSum.melt$variable == "mxT_mean_abs"] <- 
  set1[weatherSum.melt$Cluster[weatherSum.melt$variable == "mxT_mean_abs"]]
weatherSum.melt$col[weatherSum.melt$variable == "mnT_mean_abs"] <- 
  set2[weatherSum.melt$Cluster[weatherSum.melt$variable == "mnT_mean_abs"]]

# guide = FALSE idea:
# - https://stackoverflow.com/questions/14604435/turning-off-some-legends-in-a-ggplot/14604540
map8 <- ggplot(locations, aes(map_id = state)) +
  # map points to the fifty_states shape data
  geom_map(fill = alpha("gray90", 0.5), color = alpha("gray80", 0.5), 
           map = fifty_states) +
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  geom_point(aes(x = longitude2, y = latitude2, 
                 color = factor(Cluster)), size = .1) + 
  geom_polygon(data = weatherSum.melt, 
               aes(x = longitude2 + .025*value2*cos(dRad),
                   y = latitude2 + .025*value2*sin(dRad),
                   group = factor(group)),
               color = alpha(weatherSum.melt$col, 0.8), 
               fill = NA) + 
  coord_fixed() +
  ggtitle("Temperature") + 
  scale_color_manual(values =  pal2, guide = FALSE) + 
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  theme(legend.position = "none",
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(size = 18, hjust = 0.5))


# Create a custom color pallette based legend for the two colors. 
sqCol <- c(1, 2, 2, 1, 1)
sqCol <- c(sqCol, sqCol+1, sqCol+2, sqCol+3, sqCol+4, sqCol+5)
sqRow <- rep(c(1, 1, 2, 2, 1), 6)
id = c(1, 1, 1, 1, 1)
id <- c(id, id+1, id+2, id+3, id+4, id+5,
        id+6, id+7, id+8, id+9, id+10, id+11)

squareDF <- data.frame(Col = rep(sqCol, 2),
                       Row = c(sqRow, sqRow + 1),
                       id = id)

tleg <- ggplot(squareDF) + 
  geom_polygon(aes(x = Col, y = Row, group = id), fill = c(set2, set1)[id]) + 
  xlim(value = c(1, 13)) + 
  annotate("text", x = c(9, 9), y = c(2.5, 1.5), label = c("Max Temp", "Min Temp"),
           size = 4) + 
  coord_fixed() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.grid = element_blank())

tleg <- ggplotGrob(tleg)

map8 <- map8 + 
  annotation_custom(tleg, xmin = 0.15, xmax = 0.55, ymin = 0.925, ymax = 1.025)

#map8


#=============================================================================

# Map 9 - small dots/colored circles - different thickness
#=============================================================================
map9 <- ggplot(locations, aes(map_id = state)) +
  # map points to the fifty_states shape data
  geom_map(fill = alpha("gray90", 0.5), color = alpha("gray80", 0.5), 
           map = fifty_states) +
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  geom_point(aes(x = longitude2, y = latitude2, 
                 color = factor(Cluster)), size = 0.1) + 
  geom_polygon(data = weatherSum.melt, 
               aes(x = longitude2 + .02*value2*cos(dRad),
                   y = latitude2 + .02*value2*sin(dRad),
                   group = factor(group),
                   size = factor(variable), color = factor(Cluster)), 
               fill = NA) + 
  coord_fixed() +
  scale_color_manual(values =  alpha(pal2, 0.8), guide = FALSE) + 
  scale_size_manual("Absolute Error", values = c(1, 1.1),
                        labels = c("Max Temp", "Min Temp")) + 
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme(legend.position = "none",
        panel.background = element_blank())


#=============================================================================

pdf("../images/glyphPlots/glyphTempGallery.pdf", paper = "USr", width = 10, height = 8)
map
map2
map3
map4
map5
map6
map7
map8
map9
dev.off()




