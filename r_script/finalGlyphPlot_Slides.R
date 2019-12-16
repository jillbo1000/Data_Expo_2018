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

# Determine minimum and maximum Brier Skill Scores (unajusted)
minp <- min(1-weatherSum$BSS)
maxp <- max(1-weatherSum$BSS)

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

# Map 8 - small dots, different line thickness/intensity for each cluster. 
#=============================================================================
set1 <- c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#f781bf")
set1 <- set1[c(4, 2, 3, 6, 1, 5)] # Reorder to match the original layout
set2 <- c("#fb9a99", "#a6cee3", "#b2df8a", "#cab2d6", "#fdbf6f", "#fccde5")
set2 <- set2[c(4, 2, 3, 6, 1, 5)] # Reorder to match the original layout

weatherSum.melt$col <- "black"
weatherSum.melt$col2 <- "black"
weatherSum.melt$col[weatherSum.melt$variable == "mxT_mean_abs"] <- 
  set1[weatherSum.melt$Cluster[weatherSum.melt$variable == "mxT_mean_abs"]]
weatherSum.melt$col[weatherSum.melt$variable == "mnT_mean_abs"] <- 
  set2[weatherSum.melt$Cluster[weatherSum.melt$variable == "mnT_mean_abs"]]
weatherSum.melt$col2[weatherSum.melt$variable == "mnT_mean_abs"] <- 
  set1[weatherSum.melt$Cluster[weatherSum.melt$variable == "mnT_mean_abs"]]

# guide = FALSE idea:
# - https://stackoverflow.com/questions/14604435/turning-off-some-legends-in-a-ggplot/14604540
locations$Type <- "Temperature"
map8 <- ggplot(locations, aes(map_id = state)) +
  # map points to the fifty_states shape data
  geom_map(fill = alpha("gray80", 0.5), color = alpha("gray60", 0.5), 
           map = fifty_states) +
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  geom_point(aes(x = longitude2, y = latitude2, 
                 color = factor(Cluster)), size = .1) + 
  geom_polygon(data = weatherSum.melt, 
               aes(x = longitude2 + .025*value2*cos(dRad),
                   y = latitude2 + .025*value2*sin(dRad),
                   group = factor(group)),
               color = alpha(weatherSum.melt$col, 0.9), 
               fill = NA) + 
  coord_fixed() +
  facet_grid(~Type) + 
  scale_color_manual(values =  pal2) + 
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  theme(legend.position = "none",
        #panel.background = element_rect(fill = NA, color = "gray"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        strip.text = element_text(size = 25),
        plot.margin = unit(c(0, 0, 0, 0 ), "cm"))


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

set2.temp <- set2[c(4, 3, 2, 5, 1, 6)]
set1.temp <- set1[c(4, 3, 2, 5, 1, 6)]
tleg <- ggplot(squareDF) + 
  geom_polygon(aes(x = Col, y = Row, group = id), fill = c(set2.temp, set1.temp)[id]) + 
  xlim(value = c(1, 13)) + 
  annotate("text", x = c(9, 9), y = c(2.5, 1.5), label = c("Max Temp", "Min Temp"),
           size = 6) + 
  coord_fixed() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

tleg <- ggplotGrob(tleg)

map8 <- map8 + 
  annotation_custom(tleg, xmin = 0.16, xmax = 0.56, ymin = 0.92, ymax = 1.02)
#=============================================================================


# Precip Glyph
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
  scale_color_manual(values = alpha(pal2, 0.9)) + 
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  theme(legend.position = "none",
        #panel.background = element_rect(fill = NA, color = "gray"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        strip.text = element_text(size = 25),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))
#=============================================================================
locations$Type <- "Temperature"
map8.2 <- ggplot(locations, aes(map_id = state)) +
  # map points to the fifty_states shape data
  geom_map(fill = alpha("gray80", 0.5), color = alpha("gray60", 0.5), 
           map = fifty_states) +
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  geom_point(aes(x = longitude2, y = latitude2, 
                 color = factor(Cluster)), size = .1) + 
  geom_polygon(data = weatherSum.melt, 
               aes(x = longitude2 + .025*value2*cos(dRad),
                   y = latitude2 + .025*value2*sin(dRad),
                   group = factor(group)),
               color = alpha(weatherSum.melt$col, 0.9), 
               fill = NA) + 
  coord_fixed() +
  facet_grid(~Type) + 
  scale_color_manual(values =  pal2) + 
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  theme(legend.position = "none",
        #panel.background = element_rect(fill = NA, color = "gray"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        strip.text = element_text(size = 35),
        plot.margin = unit(c(0, 0, 0, 0 ), "cm"))


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

set2.temp <- set2[c(4, 3, 2, 5, 1, 6)]
set1.temp <- set1[c(4, 3, 2, 5, 1, 6)]
tleg <- ggplot(squareDF) + 
  geom_polygon(aes(x = Col, y = Row, group = id), fill = c(set2.temp, set1.temp)[id]) + 
  xlim(value = c(1, 13)) + 
  annotate("text", x = c(9, 9), y = c(2.5, 1.5), label = c("Max Temp", "Min Temp"),
           size = 8) + 
  coord_fixed() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

tleg <- ggplotGrob(tleg)

map8.2 <- map8.2 + 
  annotation_custom(tleg, xmin = 0.16, xmax = 0.56, ymin = 0.92, ymax = 1.02)
#=============================================================================


# Precip Glyph
#=============================================================================
locations$Type <- "Precipitation"
map.2 <- ggplot(locations, aes(map_id = state)) +
  # map points to the fifty_states shape data
  geom_map(fill = alpha("gray80", 0.5), color = alpha("gray60", 0.5), 
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
  scale_color_manual(values = alpha(pal2, 0.9)) + 
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  theme(legend.position = "none",
        #panel.background = element_rect(fill = NA, color = "gray"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        strip.text = element_text(size = 35),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))
#=============================================================================

# Rose Plots
marAdj <- c(0, 0, 0, 0)
#=============================================================================
# Maximum Temperature
#=============================================================================
maxSub <- weatherSum.melt[weatherSum.melt$variable == "mxT_mean_abs", ]
maxSub$Type <- "Max Temp"
minSub <- weatherSum.melt[weatherSum.melt$variable == "mnT_mean_abs", ]
minSub$Type <- "Min Temp"
tRad <- seq(0, 2*pi, length.out = 1000)
tRad <- data.frame(tRad = tRad)

adj1 <- 2.5
adj1.1 <- 5
adj2 <- adj1*(maxp/maxEr)
adj2.1 <- adj1.1*(maxp/maxEr)
tglyph <- ggplot(maxSub) +
  xlim(-maxEr - adj1.1 - 7, maxEr + adj1.1 + 7) + 
  ylim(-maxEr - adj1, maxEr + adj1) + 
  geom_segment(data = maxSub, 
               aes(x = 0, xend = maxEr*cos(dRad),
                   y = 0, yend = maxEr*sin(dRad),
                   group = factor(dRad)),
               col = alpha("gray", 0.1), lwd = 0.1) + 
  annotate("text", x = (maxEr + 2)*cos(unique(maxSub$dRad)), 
           y = (maxEr + 2)*sin(unique(maxSub$dRad)),
           label = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", 
                     "Sep", "Oct", "Nov", "Dec"),
           size = 7, col = "gray") + 
  geom_path(data = tRad, 
            aes(x = minEr*cos(tRad), y = minEr*sin(tRad)), 
            inherit.aes = FALSE,
            col = alpha("gray", 0.9)) + 
  geom_path(data = tRad, 
            aes(x = maxEr*cos(tRad), y = maxEr*sin(tRad)), 
            inherit.aes = FALSE,
            col = alpha("gray", 0.9)) + 
  geom_polygon(data = maxSub, 
               aes(x = value*cos(dRad),
                   y = value*sin(dRad),
                   group = factor(group)),
               inherit.aes = FALSE,
               color = alpha(maxSub$col, 0.4), 
               fill = NA) + 
  facet_grid(~Type) + 
  geom_segment(aes(y = 0, yend = maxEr, x = -maxEr - adj1.1, xend = -maxEr - adj1.1), size = 0.5) + 
  annotate("text", x = c(-maxEr - adj1.1, -maxEr - adj1.1), y= c(minEr, maxEr), 
           label = as.character(round(c(minEr, maxEr), 2)),
           hjust = 1.25, size = 6) + 
  annotate("text", x = -maxEr - adj1.1, y = (minEr + maxEr) / 2, 
           label = "Abs Error", angle = 90,
           vjust = -.5, size = 5) + 
  annotate("point", x = rep(-maxEr - adj1.1, 3)-.1, y = c(0, minEr, maxEr)-.1, pch = "-", size = 6) + 
  coord_fixed() +
  theme(legend.position = "none",
        #panel.background = element_rect(fill = NA, color = "gray"),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        #plot.title = element_text(size = 25, hjust = 0.5),
        strip.text = element_text(size = 25),
        plot.margin = unit(marAdj, "cm"))
#=============================================================================

# Minimum Temperature
#=============================================================================
tglyph2 <- ggplot(minSub) +
  #geom_point(aes(x = 0, y= 0)) +
  xlim(-maxEr - adj1.1 - 7, maxEr + adj1.1 + 7) + 
  ylim(-maxEr - adj1, maxEr + adj1) + 
  geom_segment(data = minSub, 
               aes(x = 0, xend = maxEr*cos(dRad),
                   y = 0, yend = maxEr*sin(dRad),
                   group = factor(dRad)),
               col = alpha("gray", 0.1), lwd = 0.1) + 
  annotate("text", x = (maxEr + 2)*cos(unique(minSub$dRad)), 
           y = (maxEr + 2)*sin(unique(minSub$dRad)),
           label = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", 
                     "Sep", "Oct", "Nov", "Dec"),
           size = 7, col = "gray") + 
  geom_path(data = tRad, 
            aes(x = minEr*cos(tRad), y = minEr*sin(tRad)), 
            inherit.aes = FALSE,
            col = alpha("gray", 0.9)) + 
  geom_path(data = tRad, 
            aes(x = maxEr*cos(tRad), y = maxEr*sin(tRad)), 
            inherit.aes = FALSE,
            col = alpha("gray", 0.9)) + 
  geom_polygon(data = minSub, 
               aes(x = value*cos(dRad),
                   y = value*sin(dRad),
                   group = factor(group)),
               inherit.aes = FALSE,
               color = alpha(minSub$col2, 0.4), 
               fill = NA) + 
  geom_segment(aes(y = 0, yend = maxEr, x = -maxEr - adj1.1, xend = -maxEr - adj1.1), size = 0.5) + 
  annotate("text", x = c(-maxEr - adj1.1, -maxEr - adj1.1), y= c(minEr, maxEr), 
           label = as.character(round(c(minEr, maxEr), 2)),
           hjust = 1.25, size = 6) + 
  annotate("text", x = -maxEr - adj1.1, y = (minEr + maxEr) / 2, 
           label = "Abs Error", angle = 90,
           vjust = -.5, size = 5) + 
  annotate("point", x = rep(-maxEr - adj1.1, 3)-.1, 
           y = c(0, minEr, maxEr)-.1, pch = "-", size = 6) + 
  coord_fixed() +
  facet_grid(~Type) + 
  theme(legend.position = "none",
        #panel.background = element_rect(fill = NA, color = "gray"),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        #plot.title = element_text(size = 18, hjust = 0.5),
        strip.text = element_text(size = 25),
        plot.margin = unit(marAdj, "cm"))
#=============================================================================

# Precipitation
#=============================================================================
weatherSum$PrecipType <- "Precip"
tglyph3 <- ggplot(weatherSum) +
  #geom_point(aes(x = 0, y= 0)) +
  xlim(-maxp - adj2.1 - 7*(maxp/maxEr), maxp + adj2.1 + 7*(maxp/maxEr)) + 
  ylim(-maxp - adj2, maxp + adj2) + 
  geom_segment(data = weatherSum, 
               aes(x = 0, xend = maxp*cos(dRad),
                   y = 0, yend = maxp*sin(dRad),
                   group = factor(dRad)),
               col = alpha("gray", 0.1), lwd = 0.1) + 
  annotate("text", x = (maxp + .6)*cos(unique(weatherSum$dRad)), 
           y = (maxp + .6)*sin(unique(weatherSum$dRad)),
           label = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", 
                     "Sep", "Oct", "Nov", "Dec"),
           size = 7, col = "gray") + 
  geom_path(data = tRad, 
            aes(x = minp*cos(tRad), y = minp*sin(tRad)), 
            inherit.aes = FALSE,
            col = alpha("gray", 0.9)) + 
  geom_path(data = tRad, 
            aes(x = maxp*cos(tRad), y = maxp*sin(tRad)), 
            inherit.aes = FALSE,
            col = alpha("gray", 0.9)) + 
  geom_polygon(data = weatherSum, 
               aes(x = (1-BSS)*cos(dRad),
                   y = (1-BSS)*sin(dRad),
                   group = factor(AirPtCd),
                   color = factor(Cluster)),
               inherit.aes = FALSE,
               fill = NA) +
  geom_segment(aes(y = 0, yend = maxp, x = -maxp - adj2.1, xend = -maxp - adj2.1), size = 0.5) + 
  annotate("text", x = c(-maxp - adj2.1, -maxp - adj2.1), y= c(minp, maxp), 
           label = as.character(round(c(minp, maxp), 2)),
           hjust = 1.5, size = 6) + 
  annotate("text", x = -maxp - adj2.1, y = (minp + maxp) / 2, 
           label = "1 - BSS", angle = 90,
           vjust = -.5, size = 5) + 
  annotate("point", x = rep(-maxp - adj2.1, 3)-.1*(maxp/maxEr), 
           y = c(0, minp, maxp)-.1*(maxp/maxEr), pch = "-", size = 6) + 
  facet_grid(~PrecipType) +
  coord_fixed() +
  scale_color_manual(values =  alpha(pal2, 0.4), guide = FALSE) + 
  theme(legend.position = "none",
        #panel.background = element_rect(fill = NA, color = "gray"),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        #plot.title = element_text(size = 18, hjust = 0.5),
        strip.text = element_text(size = 26),
        plot.margin = unit(marAdj, "cm"))

#=============================================================================


# Rose Plots
marAdj <- c(0, 0, 0, 0)
#=============================================================================
# Maximum Temperature
#=============================================================================
maxSub <- weatherSum.melt[weatherSum.melt$variable == "mxT_mean_abs", ]
maxSub$Type <- "Max Temp"
minSub <- weatherSum.melt[weatherSum.melt$variable == "mnT_mean_abs", ]
minSub$Type <- "Min Temp"
tRad <- seq(0, 2*pi, length.out = 1000)
tRad <- data.frame(tRad = tRad)

adj1 <- 2.5
adj1.1 <- 5
adj2 <- adj1*(maxp/maxEr)
adj2.1 <- adj1.1*(maxp/maxEr)
tglyph.2 <- ggplot(maxSub) +
  xlim(-maxEr - adj1.1 - 5, maxEr + adj1.1 + 5) + 
  ylim(-maxEr - adj1, maxEr + adj1) + 
  geom_segment(data = maxSub, 
               aes(x = 0, xend = maxEr*cos(dRad),
                   y = 0, yend = maxEr*sin(dRad),
                   group = factor(dRad)),
               col = alpha("gray", 0.1), lwd = 0.1) + 
  annotate("text", x = (maxEr + 2)*cos(unique(maxSub$dRad)), 
           y = (maxEr + 2)*sin(unique(maxSub$dRad)),
           label = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", 
                     "Sep", "Oct", "Nov", "Dec"),
           size = 8, col = "gray") + 
  geom_path(data = tRad, 
            aes(x = minEr*cos(tRad), y = minEr*sin(tRad)), 
            inherit.aes = FALSE,
            col = alpha("gray", 0.9)) + 
  geom_path(data = tRad, 
            aes(x = maxEr*cos(tRad), y = maxEr*sin(tRad)), 
            inherit.aes = FALSE,
            col = alpha("gray", 0.9)) + 
  geom_polygon(data = maxSub, 
               aes(x = value*cos(dRad),
                   y = value*sin(dRad),
                   group = factor(group)),
               inherit.aes = FALSE,
               color = alpha(maxSub$col, 0.4), 
               fill = NA) + 
  facet_grid(~Type) + 
  geom_segment(aes(y = 0, yend = maxEr, x = -maxEr - adj1.1, xend = -maxEr - adj1.1), size = 0.5) + 
  annotate("text", x = c(-maxEr - adj1.1, -maxEr - adj1.1), y= c(minEr, maxEr), 
           label = as.character(round(c(minEr, maxEr), 2)),
           hjust = 1.25, size = 8) + 
  annotate("text", x = -maxEr - adj1.1, y = (minEr + maxEr) / 2, 
           label = "Abs Error", angle = 90,
           vjust = -.5, size = 6) + 
  annotate("point", x = rep(-maxEr - adj1.1, 3)-.1, y = c(0, minEr, maxEr)-.1, pch = "-", size = 6) + 
  coord_fixed() +
  theme(legend.position = "none",
        #panel.background = element_rect(fill = NA, color = "gray"),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        #plot.title = element_text(size = 25, hjust = 0.5),
        strip.text = element_text(size = 35),
        plot.margin = unit(marAdj, "cm"))
#=============================================================================

# Minimum Temperature
#=============================================================================
tglyph2.2 <- ggplot(minSub) +
  #geom_point(aes(x = 0, y= 0)) +
  xlim(-maxEr - adj1.1 - 5, maxEr + adj1.1 + 5) + 
  ylim(-maxEr - adj1, maxEr + adj1) + 
  geom_segment(data = minSub, 
               aes(x = 0, xend = maxEr*cos(dRad),
                   y = 0, yend = maxEr*sin(dRad),
                   group = factor(dRad)),
               col = alpha("gray", 0.1), lwd = 0.1) + 
  annotate("text", x = (maxEr + 2)*cos(unique(minSub$dRad)), 
           y = (maxEr + 2)*sin(unique(minSub$dRad)),
           label = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", 
                     "Sep", "Oct", "Nov", "Dec"),
           size = 8, col = "gray") + 
  geom_path(data = tRad, 
            aes(x = minEr*cos(tRad), y = minEr*sin(tRad)), 
            inherit.aes = FALSE,
            col = alpha("gray", 0.9)) + 
  geom_path(data = tRad, 
            aes(x = maxEr*cos(tRad), y = maxEr*sin(tRad)), 
            inherit.aes = FALSE,
            col = alpha("gray", 0.9)) + 
  geom_polygon(data = minSub, 
               aes(x = value*cos(dRad),
                   y = value*sin(dRad),
                   group = factor(group)),
               inherit.aes = FALSE,
               color = alpha(minSub$col2, 0.4), 
               fill = NA) + 
  geom_segment(aes(y = 0, yend = maxEr, x = -maxEr - adj1.1, xend = -maxEr - adj1.1), size = 0.5) + 
  annotate("text", x = c(-maxEr - adj1.1, -maxEr - adj1.1), y= c(minEr, maxEr), 
           label = as.character(round(c(minEr, maxEr), 2)),
           hjust = 1.25, size = 8) + 
  annotate("text", x = -maxEr - adj1.1, y = (minEr + maxEr) / 2, 
           label = "Abs Error", angle = 90,
           vjust = -.5, size = 6) + 
  annotate("point", x = rep(-maxEr - adj1.1, 3)-.1, 
           y = c(0, minEr, maxEr)-.1, pch = "-", size = 6) + 
  coord_fixed() +
  facet_grid(~Type) + 
  theme(legend.position = "none",
        #panel.background = element_rect(fill = NA, color = "gray"),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        #plot.title = element_text(size = 18, hjust = 0.5),
        strip.text = element_text(size = 35),
        plot.margin = unit(marAdj, "cm"))
#=============================================================================

# Precipitation
#=============================================================================
weatherSum$PrecipType <- "Precip"
tglyph3.2 <- ggplot(weatherSum) +
  #geom_point(aes(x = 0, y= 0)) +
  xlim(-maxp - adj2.1 - 5*(maxp/maxEr), maxp + adj2.1 + 5*(maxp/maxEr)) + 
  ylim(-maxp - adj2, maxp + adj2) + 
  geom_segment(data = weatherSum, 
               aes(x = 0, xend = maxp*cos(dRad),
                   y = 0, yend = maxp*sin(dRad),
                   group = factor(dRad)),
               col = alpha("gray", 0.1), lwd = 0.1) + 
  annotate("text", x = (maxp + .6)*cos(unique(weatherSum$dRad)), 
           y = (maxp + .6)*sin(unique(weatherSum$dRad)),
           label = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", 
                     "Sep", "Oct", "Nov", "Dec"),
           size = 8, col = "gray") + 
  geom_path(data = tRad, 
            aes(x = minp*cos(tRad), y = minp*sin(tRad)), 
            inherit.aes = FALSE,
            col = alpha("gray", 0.9)) + 
  geom_path(data = tRad, 
            aes(x = maxp*cos(tRad), y = maxp*sin(tRad)), 
            inherit.aes = FALSE,
            col = alpha("gray", 0.9)) + 
  geom_polygon(data = weatherSum, 
               aes(x = (1-BSS)*cos(dRad),
                   y = (1-BSS)*sin(dRad),
                   group = factor(AirPtCd),
                   color = factor(Cluster)),
               inherit.aes = FALSE,
               fill = NA) +
  geom_segment(aes(y = 0, yend = maxp, x = -maxp - adj2.1, xend = -maxp - adj2.1), size = 0.5) + 
  annotate("text", x = c(-maxp - adj2.1, -maxp - adj2.1), y= c(minp, maxp), 
           label = as.character(round(c(minp, maxp), 2)),
           hjust = 1.5, size = 8) + 
  annotate("text", x = -maxp - adj2.1, y = (minp + maxp) / 2, 
           label = "Abs Error", angle = 90,
           vjust = -.5, size = 6) + 
  annotate("point", x = rep(-maxp - adj2.1, 3)-.1*(maxp/maxEr), 
           y = c(0, minp, maxp)-.1*(maxp/maxEr), pch = "-", size = 6) + 
  facet_grid(~PrecipType) +
  coord_fixed() +
  scale_color_manual(values =  alpha(pal2, 0.4), guide = FALSE) + 
  theme(legend.position = "none",
        #panel.background = element_rect(fill = NA, color = "gray"),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        #plot.title = element_text(size = 18, hjust = 0.5),
        strip.text = element_text(size = 35),
        plot.margin = unit(marAdj, "cm"))

#=============================================================================


#=============================================================================


#pdf("../images/glyphPlots/glyphTemp.pdf", paper = "USr", width = 20, height = 16)
#template <- rbind(ggplotGrob(tglyph), ggplotGrob(tglyph2), 
#      ggplotGrob(tglyph3), size = "last")
#grid.draw(cbind(ggplotGrob(map8), template, map8, size = "last"))

pdf("../images/final/glyphFinal_Slides.pdf", width = 43.35, height = 11)
grid.arrange(grobs = list(ggplotGrob(map8), ggplotGrob(tglyph),
                          ggplotGrob(tglyph2), ggplotGrob(tglyph3),
                          ggplotGrob(map)), layout_matrix = cbind(c(1,1,1), c(2, 3, 4), c(5, 5, 5)),
             widths = c(0.435, 0.13, 0.435), clip = "off")
dev.off()

pdf("../images/final/glyphFinal2_Slides.pdf", width = 20, height = 30)
grid.arrange(grobs = list(ggplotGrob(map8.2), ggplotGrob(tglyph.2), 
                          ggplotGrob(tglyph2.2), ggplotGrob(tglyph3.2), 
                          ggplotGrob(map.2)), layout_matrix = rbind(c(1,1,1), c(2, 3, 4), c(5, 5, 5)),
             heights = c(0.4, 0.2, 0.4), clip = "off")
dev.off()


#png("../images/glyphPlots/glyphTemp.png", width = 14, height = 10, units = "in", res = 300)
#map8
#dev.off()
