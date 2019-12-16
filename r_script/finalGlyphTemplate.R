# Make a sample set of reference glyphs:
# Annotation help:
# https://stackoverflow.com/questions/27585588/how-to-rotate-only-text-in-annotation-in-ggplot 
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

# Manually define the color scheme
set1 <- c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#f781bf")
set1 <- set1[c(4, 2, 3, 6, 1, 5)] # Reorder to match the original layout

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

# Create a scaled version of the BSS ratio
weatherSum$sBSS <- 1-weatherSum$BSS
weatherSum$sBSS <- weatherSum$sBSS / max(weatherSum$sBSS)

# Determine minimum and maximum Brier Skill Scores (unajusted)
minp <- min(1-weatherSum$BSS)
maxp <- max(1-weatherSum$BSS)

# Assign cluster information to clusters. 
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

weatherSum.melt$col <- "black"
weatherSum.melt$col2 <- "black"
weatherSum.melt$col[weatherSum.melt$variable == "mxT_mean_abs"] <- 
  set1[weatherSum.melt$Cluster[weatherSum.melt$variable == "mxT_mean_abs"]]
weatherSum.melt$col[weatherSum.melt$variable == "mnT_mean_abs"] <- 
  set2[weatherSum.melt$Cluster[weatherSum.melt$variable == "mnT_mean_abs"]]
weatherSum.melt$col2[weatherSum.melt$variable == "mnT_mean_abs"] <- 
  set1[weatherSum.melt$Cluster[weatherSum.melt$variable == "mnT_mean_abs"]]

# Maximum Temperature
#=============================================================================
maxSub <- weatherSum.melt[weatherSum.melt$variable == "mxT_mean_abs", ]
maxSub$Type <- "Max Temp"
minSub <- weatherSum.melt[weatherSum.melt$variable == "mnT_mean_abs", ]
minSub$Type <- "Min Temp"
tRad <- seq(0, 2*pi, length.out = 1000)
tRad <- data.frame(tRad = tRad)

adj1 <- 5
adj2 <- 1.5
tglyph <- ggplot(maxSub) +
  xlim(-maxEr - adj1 - 0.5, maxEr + adj1 + 0.5) + 
  ylim(-maxEr - adj1 - 0.5, maxEr + adj1 + 0.5) + 
  geom_segment(data = maxSub, 
               aes(x = 0, xend = maxEr*cos(dRad),
                   y = 0, yend = maxEr*sin(dRad),
                   group = factor(dRad)),
               col = alpha("gray", 0.1), lwd = 0.1) + 
  annotate("text", x = (maxEr + 2)*cos(unique(maxSub$dRad)), 
           y = (maxEr + 2)*sin(unique(maxSub$dRad)),
           label = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", 
                     "Sep", "Oct", "Nov", "Dec"),
           size = 6, col = "gray") + 
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
  geom_segment(aes(x = 0, xend = maxEr, y = -maxEr - adj1, yend = -maxEr - adj1), size = 0.5) + 
  annotate("text", y = c(-maxEr - adj1, -maxEr - adj1), x = c(minEr, maxEr), 
           label = as.character(round(c(minEr, maxEr), 2)),
           vjust = 1.6, size = 4) + 
  annotate("text", y = -maxEr - adj1, x = (minEr + maxEr) / 2, 
           label = "Abs Error",
           vjust = -.5, size = 4) + 
  annotate("point", y = rep(-maxEr - adj1, 3), x = c(0, minEr, maxEr), pch = "|", size = 2) + 
  coord_fixed() +
  theme(legend.position = "none",
        panel.background = element_rect(fill = NA, color = "gray"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 18, hjust = 0.5),
        strip.text = element_text(size = 14))
#=============================================================================

# Minimum Temperature
#=============================================================================
tglyph2 <- ggplot(minSub) +
  #geom_point(aes(x = 0, y= 0)) +
  xlim(-maxEr - adj1 - 0.5, maxEr + adj1 + 0.5) + 
  ylim(-maxEr - adj1 - 0.5, maxEr + adj1 + 0.5) + 
  geom_segment(data = minSub, 
               aes(x = 0, xend = maxEr*cos(dRad),
                   y = 0, yend = maxEr*sin(dRad),
                   group = factor(dRad)),
               col = alpha("gray", 0.1), lwd = 0.1) + 
  annotate("text", x = (maxEr + 2)*cos(unique(minSub$dRad)), 
           y = (maxEr + 2)*sin(unique(minSub$dRad)),
           label = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", 
                     "Sep", "Oct", "Nov", "Dec"),
           size = 6, col = "gray") + 
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
  geom_segment(aes(x = 0, xend = maxEr, y = -maxEr - adj1, yend = -maxEr - adj1), size = 0.5) + 
  annotate("text", y = c(-maxEr - adj1, -maxEr - adj1), x = c(minEr, maxEr), 
           label = as.character(round(c(minEr, maxEr), 2)),
           vjust = 1.6, size = 4) + 
  annotate("text", y = -maxEr - adj1, x = (minEr + maxEr) / 2, 
           label = "Abs Error",
           vjust = -.5, size = 4) + 
  annotate("point", y = rep(-maxEr - adj1, 3), x = c(0, minEr, maxEr), pch = "|", size = 2) + 
  coord_fixed() +
  facet_grid(~Type) + 
  theme(legend.position = "none",
        panel.background = element_rect(fill = NA, color = "gray"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 18, hjust = 0.5),
        strip.text = element_text(size = 14))
#=============================================================================

# Precipitation
#=============================================================================
weatherSum$PrecipType <- "Precip"
tglyph3 <- ggplot(weatherSum) +
  #geom_point(aes(x = 0, y= 0)) +
  xlim(-maxp - adj2 - .2, maxp + adj2 + .2) + 
  ylim(-maxp - adj2 - .2, maxp + adj2 + .2) + 
  geom_segment(data = weatherSum, 
               aes(x = 0, xend = maxp*cos(dRad),
                   y = 0, yend = maxp*sin(dRad),
                   group = factor(dRad)),
               col = alpha("gray", 0.1), lwd = 0.1) + 
  annotate("text", x = (maxp + .6)*cos(unique(weatherSum$dRad)), 
           y = (maxp + .6)*sin(unique(weatherSum$dRad)),
           label = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", 
                     "Sep", "Oct", "Nov", "Dec"),
           size = 6, col = "gray") + 
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
  geom_segment(aes(x = 0, xend = maxp, y = -maxp - adj2, yend = -maxp - adj2), size = 0.5) + 
  annotate("text", y = c(-maxp - adj2, -maxp - adj2), x = c(minp, maxp), 
           label = as.character(round(c(minp, maxp), 2)),
           vjust = 1.6, size = 4) + 
  annotate("text", y = -maxp - adj2, x = (minp + maxp) / 2, 
           label = "1 - BSS",
           vjust = -.5, size = 4) + 
  annotate("point", y = rep(-maxp - adj2, 3), x = c(0, minp, maxp), pch = "|", size = 2) + 
  facet_grid(~PrecipType) +
  coord_fixed() +
  scale_color_manual(values =  alpha(pal2, 0.4), guide = FALSE) + 
  theme(legend.position = "none",
        panel.background = element_rect(fill = NA, color = "gray"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 18, hjust = 0.5),
        strip.text = element_text(size = 14))

#=============================================================================



pdf("../images/glyphPlots/glyphCombinedMax.pdf", width = 13, height = 4)
grid.draw(cbind(ggplotGrob(tglyph), ggplotGrob(tglyph2), 
                ggplotGrob(tglyph3), size = "last"))
dev.off()


png("../images/glyphPlots/glyphCombinedMax.png", width = 13, height = 4, units = "in", res = 300)
grid.draw(cbind(ggplotGrob(tglyph), ggplotGrob(tglyph2), 
                ggplotGrob(tglyph3), size = "last"))
dev.off()


