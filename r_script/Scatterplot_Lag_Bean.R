# Set working directory to source file location prior to running. 

library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(ggmap)
library(fiftystater)

# Read in weather, forecast, and city data. 
# scatter <- read.csv("../Data/summary_city.csv")

# We changed data files to one that had all of the lags. lag=10 is the data aggregated 
# over all of the lags.
scatter <- read.csv("../Data/summary_lag_shiny.csv")
summary(scatter)
colnames(scatter)
dim(scatter)
scatter$BSS <- 1 - scatter$BSS

clust = read.csv("../Data/locationsFinal.csv")

scatter = left_join(scatter, clust)
scatter$state = tolower(scatter$state)

longitude = c(-117.25, -108.50)
latitude = c(27.2, 27)
scatter$latitude[scatter$state == "alaska"] = latitude[1]
scatter$latitude[scatter$state == "hawaii"] = latitude[2]
scatter$longitude[scatter$state == "alaska"] = longitude[1]
scatter$longitude[scatter$state == "hawaii"] = longitude[2]


#===========================================================================
# Scatterplots for data outliers
#===========================================================================

# Initial plot aggregated over lags

agg = scatter[scatter$lag == 10, ]
agg1 =scatter[scatter$lag == 1, ]
agg5 =scatter[scatter$lag == 5, ]

# Bad on min/max
s1 = agg[agg$mnT_mean_abs > 5, ]

# precip extremes
s2 = agg[agg$BSS > 1.43, ]

# good with min/max
s3 = agg[(agg$mxT_mean_abs < 1.5 | agg$mxT_mean_abs > 6), ]

g1 = ggplot(agg5, aes(x = mnT_mean_abs, y = BSS)) + 
  geom_point(color = agg$color, alpha = 0.4) +
  geom_segment(x = agg5$mnT_mean_abs, xend = agg1$mnT_mean_abs,
               y = agg5$BSS, yend = agg1$BSS, color = agg$color,
               arrow = arrow(type = "closed", length = unit(0.015, "npc"))) + 
  #geom_point(data = s1, fill = s1$color, shape = 21, size = 3) +
  theme_bw() +
  coord_fixed(ratio = 2.8, xlim = c(2, 8), ylim = c(0.4, 2.7), expand = TRUE) +
  scale_x_continuous(breaks = c(2:8)) +
  scale_y_continuous(breaks = c(seq(0.5, 2.5, 0.5))) +
  labs(x = "Minimum Temperature (F)", y = "Precipitation (1-BSS)")



g2 = ggplot(agg, aes(x = mxT_mean_abs, y = BSS)) + 
  geom_point(color = agg$color, alpha = 0.4) +
  geom_point(data = s2, fill = s2$color, shape = 21, size = 3) +
  theme_bw() +
  coord_fixed(ratio = 2.6, xlim = c(1.25, 7), ylim = c(0.4, 2.7), expand = TRUE) +
  scale_x_continuous(breaks = c(1:7)) +
  scale_y_continuous(breaks = c(seq(0.5, 2.5, 0.5))) +
  labs(x = "Maximum Temperature (F)", y = "Precipitation (1-BSS)")

g3 = ggplot(agg, aes(x = mnT_mean_abs, y = mxT_mean_abs)) + 
  geom_point(color = agg$color, alpha = 0.4) +
  geom_point(data = s3, fill = s3$color, shape = 21, size = 3) +
  theme_bw() +
  coord_fixed(ratio = 1.1, xlim = c(2, 8), ylim = c(1.25, 7), expand = TRUE) +
  scale_x_continuous(breaks = c(2:8)) +
  scale_y_continuous(breaks = c(1:7)) +
  labs(x = "Minimum Temperature (F)", y = "Maximum Temperature (F)")

#grid.arrange(g1, g2, g3, nrow = 1)

g4 = ggplot(agg, aes(map_id = state)) +
  # map points to the fifty_states shape data
  geom_map(fill = "gray90", color = "gray80", map = fifty_states) +
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  geom_point(aes(x = longitude, y = latitude), color = agg$color, size = 1, alpha = 0.4) +
  geom_point(data = s1, aes(x = longitude, y = latitude),
             fill = s1$color, shape = 21, size = 3) +
  scale_x_continuous(breaks = NULL) +
  theme(legend.position = "bottom",
        panel.background = element_blank()) 

g5 = ggplot(agg, aes(map_id = state)) +
  # map points to the fifty_states shape data
  geom_map(fill = "gray90", color = "gray80", map = fifty_states) +
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  geom_point(aes(x = longitude, y = latitude), color = agg$color, size = 1, alpha = 0.4) +
  geom_point(data = s2, aes(x = longitude, y = latitude),
             fill = s2$color, shape = 21, size = 3) +
  theme(legend.position = "bottom",
        panel.background = element_blank()) 

g6 = ggplot(agg, aes(map_id = state)) +
  # map points to the fifty_states shape data
  geom_map(fill = "gray90", color = "gray80", map = fifty_states) +
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  geom_point(aes(x = longitude, y = latitude), color = agg$color, size = 1, alpha = 0.4) +
  geom_point(data = s3, aes(x = longitude, y = latitude),
             fill = s3$color, shape = 21, size = 3) +
  theme(legend.position = "bottom",
        panel.background = element_blank()) 

grid.draw(rbind(ggplotGrob(g1), ggplotGrob(g2), ggplotGrob(g3), size = "last"))


grid.draw(rbind(ggplotGrob(p2), ggplotGrob(p1), size = "last"))

pdf("../images/Scatterplots/Final_Scatters_Portrait.pdf", height = 11, width = 8)
grid.arrange(g1, g4, g2, g5, g3, g6, widths = c(2, 3), ncol = 2)
dev.off()

pdf("../images/Scatterplots/Final_Scatters_Landscape.pdf", height = 6, width = 11)
grid.arrange(g1, g2, g3, g4, g5, g6, heights = c(2, 3), ncol = 3)
dev.off()


