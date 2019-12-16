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
scatter <- read.csv("../Data/summary_city.csv")
summary(scatter)
colnames(scatter)
dim(scatter)

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
# Scatterplots for targeted areas with cluster colors - Version 1
#===========================================================================

# Bad on min/max
s1 = scatter[scatter$mnT_mean_abs > 5, ]

# precip extremes
s2 = scatter[scatter$BSS < -0.42, ]

# good with min/max
s3 = scatter[(scatter$mxT_mean_abs < 1.5 | scatter$mxT_mean_abs > 6), ]

g1 = ggplot(scatter, aes(x = mnT_mean_abs, y = mxT_mean_abs)) + 
  geom_point(color = scatter$color, alpha = 0.4) +
  geom_point(data = s1, fill = s1$color, shape = 21, size = 3) +
  theme_bw() +
  coord_fixed(ratio = 1.2, xlim = NULL, ylim = NULL, expand = TRUE) +
  labs(x = "Minimum Temperature", y = "Maximum Temperature")

g2 = ggplot(scatter, aes(x = mxT_mean_abs, y = (-1*BSS))) + 
  geom_point(color = scatter$color, alpha = 0.4) +
  geom_point(data = s2, fill = s2$color, shape = 21, size = 3) +
  theme_bw() +
  coord_fixed(ratio = 4.2, xlim = NULL, ylim = NULL, expand = TRUE) +
  labs(x = "Maximum Temperature", y = "Precipitation")

g3 = ggplot(scatter, aes(x = mnT_mean_abs, y = mxT_mean_abs)) + 
  geom_point(color = scatter$color, alpha = 0.4) +
  geom_point(data = s3, fill = s3$color, shape = 21, size = 3) +
  theme_bw() +
  coord_fixed(ratio = 1.2, xlim = NULL, ylim = NULL, expand = TRUE) +
  labs(x = "Minimum Temperature", y = "Maximum Temperature")

#grid.arrange(g1, g2, g3, nrow = 1)

g4 = ggplot(scatter, aes(map_id = state)) +
  # map points to the fifty_states shape data
  geom_map(fill = "gray90", color = "gray80", map = fifty_states) +
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  geom_point(aes(x = longitude, y = latitude), color = scatter$color, size = 1, alpha = 0.4) +
  geom_point(data = s1, aes(x = longitude, y = latitude),
             fill = s1$color, shape = 21, size = 3) +
  scale_x_continuous(breaks = NULL) +
  theme(legend.position = "bottom",
        panel.background = element_blank()) 

g5 = ggplot(scatter, aes(map_id = state)) +
  # map points to the fifty_states shape data
  geom_map(fill = "gray90", color = "gray80", map = fifty_states) +
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  geom_point(aes(x = longitude, y = latitude), color = scatter$color, size = 1, alpha = 0.4) +
  geom_point(data = s2, aes(x = longitude, y = latitude),
             fill = s2$color, shape = 21, size = 3) +
  theme(legend.position = "bottom",
        panel.background = element_blank()) 

g6 = ggplot(scatter, aes(map_id = state)) +
  # map points to the fifty_states shape data
  geom_map(fill = "gray90", color = "gray80", map = fifty_states) +
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  geom_point(aes(x = longitude, y = latitude), color = scatter$color, size = 1, alpha = 0.4) +
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


#===========================================================================
# Scatterplots for targeted areas with cluster colors - Version 2
#===========================================================================

# Bad on min/max
s1 = scatter[(scatter$mnT_mean_abs > 5 | scatter$mnT_mean_abs < 3), ]

# precip extremes
s2 = scatter[scatter$BSS < -0.42, ]

# good with min/max
s3 = scatter[(scatter$mxT_mean_abs < 1.5 | scatter$mxT_mean_abs > 6), ]

g1 = ggplot(scatter, aes(x = mnT_mean_abs, y = mxT_mean_abs)) + 
  geom_point(color = scatter$color, alpha = 0.4) +
  geom_point(data = s1, fill = s1$color, shape = 21, size = 3) +
  theme_bw() +
  coord_fixed(ratio = 1.2, xlim = NULL, ylim = NULL, expand = TRUE) +
  labs(x = "Minimum Temperature", y = "Maximum Temperature")

g2 = ggplot(scatter, aes(x = mxT_mean_abs, y = (-1*BSS))) + 
  geom_point(color = scatter$color, alpha = 0.4) +
  geom_point(data = s2, fill = s2$color, shape = 21, size = 3) +
  theme_bw() +
  coord_fixed(ratio = 4.2, xlim = NULL, ylim = NULL, expand = TRUE) +
  labs(x = "Maximum Temperature", y = "Precipitation")

g3 = ggplot(scatter, aes(x = mnT_mean_abs, y = mxT_mean_abs)) + 
  geom_point(color = scatter$color, alpha = 0.4) +
  geom_point(data = s3, fill = s3$color, shape = 21, size = 3) +
  theme_bw() +
  coord_fixed(ratio = 1.2, xlim = NULL, ylim = NULL, expand = TRUE) +
  labs(x = "Minimum Temperature", y = "Maximum Temperature")

grid.arrange(g1, g2, g3, nrow = 1)

g4 = ggplot(scatter, aes(map_id = state)) +
  # map points to the fifty_states shape data
  geom_map(fill = "gray90", color = "gray80", map = fifty_states) +
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  geom_point(aes(x = longitude, y = latitude), color = scatter$color, size = 1, alpha = 0.4) +
  geom_point(data = s1, aes(x = longitude, y = latitude),
             fill = s1$color, shape = 21, size = 3) +
  scale_x_continuous(breaks = NULL) +
  theme(legend.position = "bottom",
        panel.background = element_blank()) 

g5 = ggplot(scatter, aes(map_id = state)) +
  # map points to the fifty_states shape data
  geom_map(fill = "gray90", color = "gray80", map = fifty_states) +
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  geom_point(aes(x = longitude, y = latitude), color = scatter$color, size = 1, alpha = 0.4) +
  geom_point(data = s2, aes(x = longitude, y = latitude),
             fill = s2$color, shape = 21, size = 3) +
  theme(legend.position = "bottom",
        panel.background = element_blank()) 

g6 = ggplot(scatter, aes(map_id = state)) +
  # map points to the fifty_states shape data
  geom_map(fill = "gray90", color = "gray80", map = fifty_states) +
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  geom_point(aes(x = longitude, y = latitude), color = scatter$color, size = 1, alpha = 0.4) +
  geom_point(data = s3, aes(x = longitude, y = latitude),
             fill = s3$color, shape = 21, size = 3) +
  theme(legend.position = "bottom",
        panel.background = element_blank()) 


pdf("../images/Scatterplots/Final_Scatters_Portrait_v2.pdf", height = 11, width = 8)
grid.arrange(g1, g4, g2, g5, g3, g6, widths = c(2, 3), ncol = 2)
dev.off()

pdf("../images/Scatterplots/Final_Scatters_Landscape_v2.pdf", height = 6, width = 11)
grid.arrange(g1, g2, g3, g4, g5, g6, heights = c(2, 3), ncol = 3)
dev.off()

pdf("../images/Scatterplots/Final_Scatters_Landscape_v3.pdf", height = 6, width = 25)
grid.draw(cbind(ggplotGrob(g1), ggplotGrob(g4), ggplotGrob(g2), ggplotGrob(g5), 
                ggplotGrob(g3), ggplotGrob(g6), size = "last"))
dev.off()

#===========================================================================
#===========================================================================
#===========================================================================
# Older Versions of Scatterplots
#===========================================================================
#===========================================================================
#===========================================================================

#===========================================================================
# Scatterplots for Max Errors
#===========================================================================
scatter$col1 = 1
scatter$col1 = ifelse(scatter$mnT_mean_abs > 5, 2, scatter$col1)
scatter$col1 = ifelse(scatter$mxT_mean_abs > 4.5, 3, scatter$col1)
scatter$col1 = ifelse((scatter$mxT_mean_abs > 4.5 & scatter$mnT_mean_abs > 5), 4, scatter$col1)
s1 = scatter[scatter$col1 > 1, ]
c1 = c("gray60", "royalblue1", "red3", "purple2")

scatter$col2 = 1
scatter$col2 = ifelse(scatter$mnT_mean_abs > 5, 2, scatter$col2)
scatter$col2 = ifelse(scatter$BSS < -0.4, 3, scatter$col2)
scatter$col2 = ifelse((scatter$BSS < -0.4 & scatter$mnT_mean_abs > 5), 4, scatter$col2)
s2 = scatter[scatter$col2 > 1, ]
c2 = c("gray60", "royalblue1", "green3", "purple2")

scatter$col3 = 1
scatter$col3 = ifelse(scatter$BSS < -0.4, 2, scatter$col3)
scatter$col3 = ifelse(scatter$mxT_mean_abs > 4.5, 3, scatter$col3)
scatter$col3 = ifelse((scatter$mxT_mean_abs > 4.5 & scatter$BSS < -0.4), 4, scatter$col3)
s3 = scatter[scatter$col3 > 1, ]
c3 = c("gray60", "green3", "red3", "purple2")

g1 = ggplot(scatter, aes(x = mnT_mean_abs, y = mxT_mean_abs)) + 
  geom_point(size = 1, color = "gray45") +
  geom_point(data = s1, fill = c1[s1$col1], shape = 21, size = 2) +
  theme_bw() +
  coord_fixed(ratio = 1.2, xlim = NULL, ylim = NULL, expand = TRUE) +
  labs(x = "Minimum Temperature", y = "Maximum Temperature")

g2 = ggplot(scatter, aes(x = mnT_mean_abs, y = (-1*BSS))) + 
  geom_point(size = 1, color = "gray45") +
  geom_point(data = s2, fill = c2[s2$col2], shape = 21, size = 2) +
  theme_bw() +
  coord_fixed(ratio = 5, xlim = NULL, ylim = NULL, expand = TRUE) +
  labs(x = "Minimum Temperature", y = "Precipitation")

g3 = ggplot(scatter, aes(x = mxT_mean_abs, y = (-1*BSS))) + 
  geom_point(size = 1, color = "gray45") +
  geom_point(data = s3, fill = c3[s3$col3], shape = 21, size = 2) +
  theme_bw() +
  coord_fixed(ratio = 4.2, xlim = NULL, ylim = NULL, expand = TRUE) +
  labs(x = "Maximum Temperature", y = "Precipitation")

grid.arrange(g1, g2, g3, nrow = 1)

g4 = ggplot(scatter, aes(map_id = state)) +
  # map points to the fifty_states shape data
  geom_map(fill = "gray90", color = "gray80", map = fifty_states) +
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  geom_point(aes(x = longitude, y = latitude), size = 1, color = "gray45") +
  geom_point(data = s1, aes(x = longitude, y = latitude),
             fill = c1[s1$col1], shape = 21, size = 2) +
  # scale_color_manual(values = c("gray60", "royalblue1", "red3", "purple2"), guide = FALSE) +
  # scale_size_manual(values = c(1, 1.5)) +
  theme(legend.position = "bottom",
        panel.background = element_blank()) 

g5 = ggplot(scatter, aes(map_id = state)) +
  # map points to the fifty_states shape data
  geom_map(fill = "gray90", color = "gray80", map = fifty_states) +
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  geom_point(aes(x = longitude, y = latitude), size = 1, color = "gray45") +
  geom_point(data = s2, aes(x = longitude, y = latitude),
             fill = c2[s2$col2], shape = 21, size = 2) +
  # scale_color_manual(values = c("gray60", "royalblue1", "red3", "purple2"), guide = FALSE) +
  # scale_size_manual(values = c(1, 1.5)) +
  theme(legend.position = "bottom",
        panel.background = element_blank()) 

g6 = ggplot(scatter, aes(map_id = state)) +
  # map points to the fifty_states shape data
  geom_map(fill = "gray90", color = "gray80", map = fifty_states) +
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  geom_point(aes(x = longitude, y = latitude), size = 1, color = "gray45") +
  geom_point(data = s3, aes(x = longitude, y = latitude),
             fill = c3[s3$col3], shape = 21, size = 2) +
  # scale_color_manual(values = c("gray60", "royalblue1", "red3", "purple2"), guide = FALSE) +
  # scale_size_manual(values = c(1, 1.5)) +
  theme(legend.position = "bottom",
        panel.background = element_blank()) 

pdf("../images/Scatterplots/Max Error.pdf", height = 11, width = 8)
grid.arrange(g1, g4, g2, g5, g3, g6, widths = c(2, 3), ncol = 2)
dev.off()


#===========================================================================
# Scatterplots for Min Errors
#===========================================================================
scatter$col1 = 1
scatter$col1 = ifelse(scatter$mnT_mean_abs < 3, 2, scatter$col1)
scatter$col1 = ifelse(scatter$mxT_mean_abs < 2.6, 3, scatter$col1)
scatter$col1 = ifelse((scatter$mxT_mean_abs < 2.6 & scatter$mnT_mean_abs < 3), 4, scatter$col1)
scatter$size1 = ifelse(scatter$col1 == 1, 1, 2)
s1 = scatter[scatter$col1 > 1, ]
c1 = c("gray60", "royalblue1", "red3", "purple2")

scatter$col2 = 1
scatter$col2 = ifelse(scatter$mnT_mean_abs < 3, 2, scatter$col2)
scatter$col2 = ifelse(scatter$BSS > 0.2, 3, scatter$col2)
scatter$col2 = ifelse((scatter$BSS > 0.2 & scatter$mnT_mean_abs < 3), 4, scatter$col2)
scatter$size2 = ifelse(scatter$col2 == 1, 1, 2)
s2 = scatter[scatter$col2 > 1, ]
c2 = c("gray60", "royalblue1", "green3", "purple2")

scatter$col3 = 1
scatter$col3 = ifelse(scatter$BSS > 0.2, 2, scatter$col3)
scatter$col3 = ifelse(scatter$mxT_mean_abs < 2.6, 3, scatter$col3)
scatter$col3 = ifelse((scatter$mxT_mean_abs < 2.6 & scatter$BSS > 0.2), 4, scatter$col3)
scatter$size3 = ifelse(scatter$col3 == 1, 1, 2)
s3 = scatter[scatter$col3 > 1, ]
c3 = c("gray60", "green3", "red3", "purple2")

g1 = ggplot(scatter, aes(x = mnT_mean_abs, y = mxT_mean_abs)) + 
  geom_point(size = 1, color = "gray45") +
  geom_point(data = s1, fill = c1[s1$col1], shape = 21, size = 2) +
  theme_bw() +
  coord_fixed(ratio = 1.2, xlim = NULL, ylim = NULL, expand = TRUE) +
  labs(x = "Minimum Temperature", y = "Maximum Temperature")

g2 = ggplot(scatter, aes(x = mnT_mean_abs, y = (-1*BSS))) + 
  geom_point(size = 1, color = "gray45") +
  geom_point(data = s2, fill = c2[s2$col2], shape = 21, size = 2) +
  theme_bw() +
  coord_fixed(ratio = 5, xlim = NULL, ylim = NULL, expand = TRUE) +
  labs(x = "Minimum Temperature", y = "Precipitation")

g3 = ggplot(scatter, aes(x = mxT_mean_abs, y = (-1*BSS))) + 
  geom_point(size = 1, color = "gray45") +
  geom_point(data = s3, fill = c3[s3$col3], shape = 21, size = 2) +
  theme_bw() +
  coord_fixed(ratio = 4.2, xlim = NULL, ylim = NULL, expand = TRUE) +
  labs(x = "Maximum Temperature", y = "Precipitation")

grid.arrange(g1, g2, g3, nrow = 1)

g4 = ggplot(scatter, aes(map_id = state)) +
  # map points to the fifty_states shape data
  geom_map(fill = "gray90", color = "gray80", map = fifty_states) +
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  geom_point(aes(x = longitude, y = latitude), size = 1, color = "gray45") +
  geom_point(data = s1, aes(x = longitude, y = latitude),
             fill = c1[s1$col1], shape = 21, size = 2) +
  # scale_color_manual(values = c("gray60", "royalblue1", "red3", "purple2"), guide = FALSE) +
  # scale_size_manual(values = c(1, 1.5)) +
  theme(legend.position = "bottom",
        panel.background = element_blank()) 

g5 = ggplot(scatter, aes(map_id = state)) +
  # map points to the fifty_states shape data
  geom_map(fill = "gray90", color = "gray80", map = fifty_states) +
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  geom_point(aes(x = longitude, y = latitude), size = 1, color = "gray45") +
  geom_point(data = s2, aes(x = longitude, y = latitude),
             fill = c2[s2$col2], shape = 21, size = 2) +
  # scale_color_manual(values = c("gray60", "royalblue1", "red3", "purple2"), guide = FALSE) +
  # scale_size_manual(values = c(1, 1.5)) +
  theme(legend.position = "bottom",
        panel.background = element_blank()) 

g6 = ggplot(scatter, aes(map_id = state)) +
  # map points to the fifty_states shape data
  geom_map(fill = "gray90", color = "gray80", map = fifty_states) +
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  geom_point(aes(x = longitude, y = latitude), size = 1, color = "gray45") +
  geom_point(data = s3, aes(x = longitude, y = latitude),
             fill = c3[s3$col3], shape = 21, size = 2) +
  # scale_color_manual(values = c("gray60", "royalblue1", "red3", "purple2"), guide = FALSE) +
  # scale_size_manual(values = c(1, 1.5)) +
  theme(legend.position = "bottom",
        panel.background = element_blank()) 


pdf("../images/Scatterplots/Min Error.pdf", height = 11, width = 8)
grid.arrange(g1, g4, g2, g5, g3, g6, widths = c(2, 3), ncol = 2)
dev.off()

#===========================================================================
# Scatterplots for targeted areas
#===========================================================================

# Bad on min/max
scatter$col1 = 1
scatter$col1 = ifelse(scatter$mnT_mean_abs > 5, 2, scatter$col1)
scatter$col1 = ifelse(scatter$mxT_mean_abs > 4.5, 3, scatter$col1)
scatter$col1 = ifelse((scatter$mxT_mean_abs > 4.5 & scatter$mnT_mean_abs > 5), 4, scatter$col1)
s1 = scatter[scatter$col1 > 1, ]
s1.1 = scatter[scatter$mxT_mean_abs > 6, ]
s1.2 = scatter[(scatter$mxT_mean_abs > 4 & scatter$mnT_mean_abs < 3), ]
c1 = c("gray60", "royalblue1", "red3", "purple2")

# good with min/max
scatter$col2 = 1
scatter$col2 = ifelse(scatter$mnT_mean_abs < 3, 2, scatter$col2)
scatter$col2 = ifelse(scatter$mxT_mean_abs < 2.6, 3, scatter$col2)
scatter$col2 = ifelse((scatter$mxT_mean_abs < 2.6 & scatter$mnT_mean_abs < 3), 4, scatter$col2)
scatter$size2 = ifelse(scatter$col2 == 1, 1, 2)
s2 = scatter[scatter$col2 > 1, ]
s2.1 = scatter[scatter$mxT_mean_abs < 1.5, ]
s1.1 = scatter[scatter$mxT_mean_abs > 6, ]
c2 = c("gray60", "royalblue1", "red3", "purple2")

# precip extremes
scatter$col3 = 1
scatter$col3 = ifelse(scatter$BSS > 0.2, 2, scatter$col3)
scatter$col3 = ifelse(scatter$BSS < -0.4, 3, scatter$col3)
scatter$size3 = ifelse(scatter$col3 == 1, 1, 2)
s3 = scatter[scatter$col3 > 1, ]
c3 = c("gray60", "green3", "turquoise2")

g1 = ggplot(scatter, aes(x = mnT_mean_abs, y = mxT_mean_abs)) + 
  geom_point(size = 1, color = "gray45") +
  geom_point(data = s1, fill = c1[s1$col1], shape = 21, size = 2) +
  geom_point(data = s1.1, fill = "plum2", shape = 21, size = 3) +
  geom_point(data = s1.2, fill = "darkorange1", shape = 21, size = 3) +
  theme_bw() +
  coord_fixed(ratio = 1.2, xlim = NULL, ylim = NULL, expand = TRUE) +
  labs(x = "Minimum Temperature", y = "Maximum Temperature")

g2 = ggplot(scatter, aes(x = mnT_mean_abs, y = mxT_mean_abs)) + 
  geom_point(size = 1, color = "gray45") +
  geom_point(data = s2, fill = c2[s2$col2], shape = 21, size = 2) +
  geom_point(data = s1.2, fill = "darkorange1", shape = 21, size = 3) +
  geom_point(data = s2.1, fill = "plum2", shape = 21, size = 3) +
  theme_bw() +
  coord_fixed(ratio = 1.2, xlim = NULL, ylim = NULL, expand = TRUE) +
  labs(x = "Minimum Temperature", y = "Maximum Temperature")

g3 = ggplot(scatter, aes(x = mxT_mean_abs, y = (-1*BSS))) + 
  geom_point(size = 1, color = "gray45") +
  geom_point(data = s3, fill = c3[s3$col3], shape = 21, size = 2) +
  theme_bw() +
  coord_fixed(ratio = 4.2, xlim = NULL, ylim = NULL, expand = TRUE) +
  labs(x = "Maximum Temperature", y = "Precipitation")

grid.arrange(g1, g2, g3, nrow = 1)

g4 = ggplot(scatter, aes(map_id = state)) +
  # map points to the fifty_states shape data
  geom_map(fill = "gray90", color = "gray80", map = fifty_states) +
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  geom_point(aes(x = longitude, y = latitude), size = 1, color = "gray45") +
  geom_point(data = s1, aes(x = longitude, y = latitude),
             fill = c1[s1$col1], shape = 21, size = 2) +
  geom_point(data = s1.1, aes(x = longitude, y = latitude),
             fill = "plum2", shape = 21, size = 3) +
  geom_point(data = s1.2, aes(x = longitude, y = latitude),
             fill = "darkorange1", shape = 21, size = 3) +
  # scale_color_manual(values = c("gray60", "royalblue1", "red3", "purple2"), guide = FALSE) +
  # scale_size_manual(values = c(1, 1.5)) +
  theme(legend.position = "bottom",
        panel.background = element_blank()) 

g5 = ggplot(scatter, aes(map_id = state)) +
  # map points to the fifty_states shape data
  geom_map(fill = "gray90", color = "gray80", map = fifty_states) +
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  geom_point(aes(x = longitude, y = latitude), size = 1, color = "gray45") +
  geom_point(data = s2, aes(x = longitude, y = latitude),
             fill = c2[s2$col2], shape = 21, size = 2) +
  geom_point(data = s2.1, aes(x = longitude, y = latitude),
             fill = "plum2", shape = 21, size = 3) +
  geom_point(data = s1.2, aes(x = longitude, y = latitude),
             fill = "darkorange1", shape = 21, size = 3) +
  # scale_color_manual(values = c("gray60", "royalblue1", "red3", "purple2"), guide = FALSE) +
  # scale_size_manual(values = c(1, 1.5)) +
  theme(legend.position = "bottom",
        panel.background = element_blank()) 

g6 = ggplot(scatter, aes(map_id = state)) +
  # map points to the fifty_states shape data
  geom_map(fill = "gray90", color = "gray80", map = fifty_states) +
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  geom_point(aes(x = longitude, y = latitude), size = 1, color = "gray45") +
  geom_point(data = s3, aes(x = longitude, y = latitude),
             fill = c3[s3$col3], shape = 21, size = 2) +
  # scale_color_manual(values = c("gray60", "royalblue1", "red3", "purple2"), guide = FALSE) +
  # scale_size_manual(values = c(1, 1.5)) +
  theme(legend.position = "bottom",
        panel.background = element_blank()) 


pdf("../images/Scatterplots/Target.pdf", height = 11, width = 8)
grid.arrange(g1, g4, g2, g5, g3, g6, widths = c(2, 3), ncol = 2)
dev.off()

#===========================================================================
# Scatterplots for targeted areas with cluster colors
#===========================================================================

# Bad on min/max
scatter$col1 = 1
scatter$col1 = ifelse(scatter$mnT_mean_abs > 5, 2, scatter$col1)
scatter$col1 = ifelse(scatter$mxT_mean_abs > 4.5, 3, scatter$col1)
scatter$col1 = ifelse((scatter$mxT_mean_abs > 4.5 & scatter$mnT_mean_abs > 5), 4, scatter$col1)
s1 = scatter[scatter$col1 > 1, ]
s1.1 = scatter[scatter$mxT_mean_abs > 6, ]
s1.2 = scatter[(scatter$mxT_mean_abs > 4 & scatter$mnT_mean_abs < 3), ]
c1 = c("gray60", "royalblue1", "red3", "purple2")

# good with min/max
scatter$col2 = 1
scatter$col2 = ifelse(scatter$mnT_mean_abs < 3, 2, scatter$col2)
scatter$col2 = ifelse(scatter$mxT_mean_abs < 2.6, 3, scatter$col2)
scatter$col2 = ifelse((scatter$mxT_mean_abs < 2.6 & scatter$mnT_mean_abs < 3), 4, scatter$col2)
scatter$size2 = ifelse(scatter$col2 == 1, 1, 2)
s2 = scatter[scatter$col2 > 1, ]
s2.1 = scatter[scatter$mxT_mean_abs < 1.5, ]
s1.1 = scatter[scatter$mxT_mean_abs > 6, ]
c2 = c("gray60", "royalblue1", "red3", "purple2")

# precip extremes
scatter$col3 = 1
scatter$col3 = ifelse(scatter$BSS > 0.2, 2, scatter$col3)
scatter$col3 = ifelse(scatter$BSS < -0.4, 3, scatter$col3)
scatter$size3 = ifelse(scatter$col3 == 1, 1, 2)
s3 = scatter[scatter$col3 > 1, ]
c3 = c("gray60", "green3", "turquoise2")

g1 = ggplot(scatter, aes(x = mnT_mean_abs, y = mxT_mean_abs)) + 
  geom_point(aes(color = factor(Cluster6)), size = 1, alpha = 0.4) +
  geom_point(data = s1, fill = c1[s1$col1], shape = 21, size = 3) +
  geom_point(data = s1.1, fill = "plum2", shape = 21, size = 4) +
  geom_point(data = s1.2, fill = "darkorange1", shape = 21, size = 4) +
  scale_color_manual(values = brewer.pal(8, "Set1")[c(1:5, 8)], guide = FALSE) +
  theme_bw() +
  coord_fixed(ratio = 1.2, xlim = NULL, ylim = NULL, expand = TRUE) +
  labs(x = "Minimum Temperature", y = "Maximum Temperature")

g2 = ggplot(scatter, aes(x = mnT_mean_abs, y = mxT_mean_abs)) + 
  geom_point(aes(color = factor(Cluster6)), size = 1, alpha = 0.4) +
  geom_point(data = s2, fill = c2[s2$col2], shape = 21, size = 3) +
  geom_point(data = s1.2, fill = "darkorange1", shape = 21, size = 4) +
  geom_point(data = s2.1, fill = "plum2", shape = 21, size = 4) +
  scale_color_manual(values = brewer.pal(8, "Set1")[c(1:5, 8)], guide = FALSE) +
  theme_bw() +
  coord_fixed(ratio = 1.2, xlim = NULL, ylim = NULL, expand = TRUE) +
  labs(x = "Minimum Temperature", y = "Maximum Temperature")

g3 = ggplot(scatter, aes(x = mxT_mean_abs, y = (-1*BSS))) + 
  geom_point(aes(color = factor(Cluster6)), size = 1, alpha = 0.4) +
  geom_point(data = s3, fill = c3[s3$col3], shape = 21, size = 3) +
  scale_color_manual(values = brewer.pal(8, "Set1")[c(1:5, 8)], guide = FALSE) +
  theme_bw() +
  coord_fixed(ratio = 4.2, xlim = NULL, ylim = NULL, expand = TRUE) +
  labs(x = "Maximum Temperature", y = "Precipitation")

grid.arrange(g1, g2, g3, nrow = 1)

g4 = ggplot(scatter, aes(map_id = state)) +
  # map points to the fifty_states shape data
  geom_map(fill = "gray90", color = "gray80", map = fifty_states) +
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  geom_point(aes(x = longitude, y = latitude, color = factor(Cluster6)), size = 1, alpha = 0.4) +
  geom_point(data = s1, aes(x = longitude, y = latitude),
             fill = c1[s1$col1], shape = 21, size = 3) +
  geom_point(data = s1.1, aes(x = longitude, y = latitude),
             fill = "plum2", shape = 21, size = 4) +
  geom_point(data = s1.2, aes(x = longitude, y = latitude),
             fill = "darkorange1", shape = 21, size = 4) +
  scale_color_manual(values = brewer.pal(8, "Set1")[c(1:5, 8)], guide = FALSE) +
  scale_x_continuous(breaks = NULL) +
  # scale_color_manual(values = c("gray60", "royalblue1", "red3", "purple2"), guide = FALSE) +
  # scale_size_manual(values = c(1, 1.5)) +
  theme(legend.position = "bottom",
        panel.background = element_blank()) 

g5 = ggplot(scatter, aes(map_id = state)) +
  # map points to the fifty_states shape data
  geom_map(fill = "gray90", color = "gray80", map = fifty_states) +
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  geom_point(aes(x = longitude, y = latitude, color = factor(Cluster6)), size = 1, alpha = 0.4) +
  geom_point(data = s2, aes(x = longitude, y = latitude),
             fill = c2[s2$col2], shape = 21, size = 3) +
  geom_point(data = s2.1, aes(x = longitude, y = latitude),
             fill = "plum2", shape = 21, size = 4) +
  geom_point(data = s1.2, aes(x = longitude, y = latitude),
             fill = "darkorange1", shape = 21, size = 4) +
  scale_color_manual(values = brewer.pal(8, "Set1")[c(1:5, 8)], guide = FALSE) +
  # scale_color_manual(values = c("gray60", "royalblue1", "red3", "purple2"), guide = FALSE) +
  # scale_size_manual(values = c(1, 1.5)) +
  theme(legend.position = "bottom",
        panel.background = element_blank()) 

g6 = ggplot(scatter, aes(map_id = state)) +
  # map points to the fifty_states shape data
  geom_map(fill = "gray90", color = "gray80", map = fifty_states) +
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  geom_point(aes(x = longitude, y = latitude, color = factor(Cluster6)), size = 1, alpha = 0.4) +
  geom_point(data = s3, aes(x = longitude, y = latitude),
             fill = c3[s3$col3], shape = 21, size = 3) +
  scale_color_manual(values = brewer.pal(8, "Set1")[c(1:5, 8)], guide = FALSE) +
  # scale_color_manual(values = c("gray60", "royalblue1", "red3", "purple2"), guide = FALSE) +
  # scale_size_manual(values = c(1, 1.5)) +
  theme(legend.position = "bottom",
        panel.background = element_blank()) 


pdf("../images/Scatterplots/Target_Clust_Colors.pdf", height = 11, width = 8)
grid.arrange(g1, g4, g2, g5, g3, g6, widths = c(2, 3), ncol = 2)
dev.off()

