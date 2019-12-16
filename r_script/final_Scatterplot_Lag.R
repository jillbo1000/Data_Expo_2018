# Set working directory to source file location prior to running. 

library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(ggmap)
library(fiftystater)
library(grid)
library(cowplot)

# Control the sizes of the labels and titles
size1 = 16 # Numbers
size2 = 18 # Titles
size3 = 20 # Plot Title

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

clust <- clust[, c(6, 18:21)]

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

# Bad on min/max
s1 = agg[agg$mnT_mean_abs > 5 | agg$mnT_mean_abs < 3, ]

# precip extremes
s2 = agg[agg$BSS > 1.43, ]

# good with min/max
s3 = agg[(agg$mxT_mean_abs < 1.5 | agg$mxT_mean_abs > 6 | (agg$mnT_mean_abs < 2.5 & agg$mxT_mean_abs > 4.5)), ]

# figure for slides
s4 = agg[agg$mnT_mean_abs > 7 | agg$mnT_mean_abs < 3 | agg$BSS > 1.43, ]


g1 = ggplot(agg, aes(x = mnT_mean_abs, y = BSS)) + 
  geom_point(color = agg$color, alpha = 0.6) +
  geom_point(data = s1, fill = s1$color, shape = 21, size = 6) +
  theme_bw() +
  coord_fixed(ratio = 2.8, xlim = c(2, 8), ylim = c(0.4, 2.7), expand = TRUE) +
  scale_x_continuous(breaks = c(2:8)) +
  scale_y_continuous(breaks = c(seq(0.5, 2.5, 0.5))) +
  labs(x = "Min Temp Error (F)", y = "Precip (1-BSS)") + 
  theme( axis.text = element_text(size = size1),
         axis.title = element_text(size = size2),
         plot.title = element_text(size = size3))

g2 = ggplot(agg, aes(x = mxT_mean_abs, y = BSS)) + 
  geom_point(color = agg$color, alpha = 0.6) +
  geom_point(data = s2, fill = s2$color, shape = 21, size = 6) +
  theme_bw() +
  coord_fixed(ratio = 2.6, xlim = c(1.25, 7), ylim = c(0.4, 2.7), expand = TRUE) +
  scale_x_continuous(breaks = c(1:7)) +
  scale_y_continuous(breaks = c(seq(0.5, 2.5, 0.5))) +
  labs(x = "Max Temp Error (F)", y = "Precip (1-BSS)") + 
  theme( axis.text = element_text(size = size1),
         axis.title = element_text(size = size2),
         plot.title = element_text(size = size3))

g3 = ggplot(agg, aes(x = mnT_mean_abs, y = mxT_mean_abs)) + 
  geom_point(color = agg$color, alpha = 0.6) +
  geom_point(data = s3, fill = s3$color, shape = 21, size = 6) +
  theme_bw() +
  coord_fixed(ratio = 1.1, xlim = c(2, 8), ylim = c(1.25, 7), expand = TRUE) +
  scale_x_continuous(breaks = c(2:8)) +
  scale_y_continuous(breaks = c(1:7)) +
  labs(x = "Min Temp Error (F)", y = "Max Temp Error (F)") +
  theme( axis.text = element_text(size = size1),
         axis.title = element_text(size = size2),
         plot.title = element_text(size = size3))

gA = ggplot(agg, aes(x = mnT_mean_abs, y = BSS)) + 
  geom_point(color = agg$color, alpha = 0.6) +
  geom_point(data = s4, fill = s4$color, shape = 21, size = 6) +
  theme_bw() +
  coord_fixed(ratio = 2.8, xlim = c(2, 8), ylim = c(0.4, 2.7), expand = TRUE) +
  scale_x_continuous(breaks = c(2:8)) +
  scale_y_continuous(breaks = c(seq(0.5, 2.5, 0.5))) +
  labs(x = "Min Temp Error (F)", y = "Precip (1-BSS)") +
  theme( axis.text = element_text(size = size1),
         axis.title = element_text(size = size2),
         plot.title = element_text(size = size3))


# grid.arrange(g1, g2, g3, nrow = 1)

g4 = ggplot(agg, aes(map_id = state)) +
  # map points to the fifty_states shape data
  geom_map(fill = alpha("gray80", 0.5), color = alpha("gray60", 0.5), map = fifty_states) +
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  geom_point(aes(x = longitude, y = latitude), color = agg$color, size = 1, alpha = 0.6) +
  geom_point(data = s1, aes(x = longitude, y = latitude),
             fill = s1$color, shape = 21, size = 6) +
  scale_x_continuous(breaks = NULL) +
  theme(legend.position = "bottom",
        panel.background = element_blank(),
        axis.text = element_text(size = size1),
        axis.title = element_text(size = size2),
        plot.title = element_text(size = size3)) 

g5 = ggplot(agg, aes(map_id = state)) +
  # map points to the fifty_states shape data
  geom_map(fill = alpha("gray80", 0.5), color = alpha("gray60", 0.5), map = fifty_states) +
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  geom_point(aes(x = longitude, y = latitude), color = agg$color, size = 1, alpha = 0.6) +
  geom_point(data = s2, aes(x = longitude, y = latitude),
             fill = s2$color, shape = 21, size = 6) +
  theme(legend.position = "bottom",
        panel.background = element_blank(),
        axis.text = element_text(size = size1),
        axis.title = element_text(size = size2),
        plot.title = element_text(size = size3)) 

g6 = ggplot(agg, aes(map_id = state)) +
  # map points to the fifty_states shape data
  geom_map(fill = alpha("gray80", 0.5), color = alpha("gray60", 0.5), map = fifty_states) +
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  geom_point(aes(x = longitude, y = latitude), color = agg$color, size = 1, alpha = 0.6) +
  geom_point(data = s3, aes(x = longitude, y = latitude),
             fill = s3$color, shape = 21, size = 6) +
  theme(legend.position = "bottom",
        panel.background = element_blank(),
        axis.text = element_text(size = size1),
        axis.title = element_text(size = size2),
        plot.title = element_text(size = size3)) 

gB = ggplot(agg, aes(map_id = state)) +
  # map points to the fifty_states shape data
  geom_map(fill = alpha("gray80", 0.5), color = alpha("gray60", 0.5), map = fifty_states) +
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  geom_point(aes(x = longitude, y = latitude), color = agg$color, size = 1, alpha = 0.6) +
  geom_point(data = s4, aes(x = longitude, y = latitude),
             fill = s4$color, shape = 21, size = 5) +
  theme(legend.position = "bottom",
        panel.background = element_blank(),
        axis.text = element_text(size = size1),
        axis.title = element_text(size = size2),
        plot.title = element_text(size = size3)) 


grid.draw(cbind(ggplotGrob(g1), ggplotGrob(g2), ggplotGrob(g3), size = "last"))


pdf("../images/Scatterplots/AggMap1.pdf", height = 5, width = 11)
grid.arrange(g1, g4, widths = c(2, 4), ncol = 2)
dev.off()

pdf("../images/Scatterplots/AggMap2.pdf", height = 5, width = 11)
grid.arrange(g2, g5, widths = c(2, 4), ncol = 2)
dev.off()

pdf("../images/Scatterplots/AggMap3.pdf", height = 5, width = 11)
grid.arrange(g3, g6, widths = c(2, 4), ncol = 2)
dev.off()


pdf("../images/Scatterplots/Final_Scatters_Portrait.pdf", height = 11, width = 8)
grid.arrange(g1, g4, g2, g5, g3, g6, widths = c(2, 3), ncol = 2)
dev.off()

pdf("../images/Scatterplots/Final_Scatters_Landscape.pdf", height = 6, width = 11)
grid.arrange(g1, g2, g3, g4, g5, g6, heights = c(2, 3), ncol = 3)
dev.off()


#===========================================================================
# Scatterplots for forecast lags
#===========================================================================

# Select data for lags 1, 3, 5 for the graphs
lags <- scatter[is.element(scatter$lag, c(1, 3, 5)), ]

# set up parameters for plotting. I don't end up using all of these, but I left
# them in case we need them later. 
lags$alphaP1 <- ifelse(is.element(as.character(lags$AirPtCd), as.character(s1$AirPtCd)), 1, 0.6)
lags$sizeP1 <- ifelse(is.element(as.character(lags$AirPtCd), as.character(s1$AirPtCd)), 6, 1)
lags$shapeP1 <- ifelse(is.element(as.character(lags$AirPtCd), as.character(s1$AirPtCd)), 21, 16)
lags$bordP1 <- ifelse(is.element(as.character(lags$AirPtCd), as.character(s1$AirPtCd)), "black", lags$color)

lags$alphaP2 <- ifelse(is.element(as.character(lags$AirPtCd), as.character(s2$AirPtCd)), 1, 0.6)
lags$sizeP2 <- ifelse(is.element(as.character(lags$AirPtCd), as.character(s2$AirPtCd)), 6, 1)
lags$shapeP2 <- ifelse(is.element(as.character(lags$AirPtCd), as.character(s2$AirPtCd)), 21, 16)
lags$bordP2 <- ifelse(is.element(as.character(lags$AirPtCd), as.character(s2$AirPtCd)), "black", lags$color)

lags$alphaP3 <- ifelse(is.element(as.character(lags$AirPtCd), as.character(s3$AirPtCd)), 1, 0.6)
lags$sizeP3 <- ifelse(is.element(as.character(lags$AirPtCd), as.character(s3$AirPtCd)), 6, 1)
lags$shapeP3 <- ifelse(is.element(as.character(lags$AirPtCd), as.character(s3$AirPtCd)), 21, 16)
lags$bordP3 <- ifelse(is.element(as.character(lags$AirPtCd), as.character(s3$AirPtCd)), "black", lags$color)

lags$alphaPA <- ifelse(is.element(as.character(lags$AirPtCd), as.character(s4$AirPtCd)), 1, 0.6)
lags$sizePA <- ifelse(is.element(as.character(lags$AirPtCd), as.character(s4$AirPtCd)), 4, 1)
lags$shapePA <- ifelse(is.element(as.character(lags$AirPtCd), as.character(s4$AirPtCd)), 21, 16)
lags$bordPA <- ifelse(is.element(as.character(lags$AirPtCd), as.character(s4$AirPtCd)), "black", lags$color)

lag1 <- lags[lags$lag == 1, ]
lag3 <- lags[lags$lag == 3, ]
lag5 <- lags[lags$lag == 5, ]

# Plots showing bad min temps
lagP1.l1 <- ggplot(lag1, aes(x = mnT_mean_abs, y = BSS)) + 
  geom_point(color = lag1$color, alpha = lag1$alphaP1, size = lag1$sizeP1) +
  theme_bw() +
  coord_fixed(ratio = 2.8, xlim = c(2, 8), ylim = c(0.4, 2.7), expand = TRUE) +
  scale_x_continuous(breaks = c(2:8)) +
  scale_y_continuous(breaks = c(seq(0.5, 2.5, 0.5))) +
  labs(title = "Forecast Lag 1", x = "", y = "") + 
  theme(axis.text.y = element_blank(), plot.margin=unit(c(1, 1, 1, -0.10), "cm"),
        axis.text = element_text(size = size1),
        axis.title = element_text(size = size2),
        plot.title = element_text(size = size3))

lagP1.l3 <- ggplot(lag3, aes(x = mnT_mean_abs, y = BSS)) + 
  geom_point(color = lag3$color, alpha = lag3$alphaP1, size = lag3$sizeP1) +
  theme_bw() +
  coord_fixed(ratio = 2.8, xlim = c(2, 8), ylim = c(0.4, 2.7), expand = TRUE) +
  scale_x_continuous(breaks = c(2:8)) +
  scale_y_continuous(breaks = c(seq(0.5, 2.5, 0.5))) +
  labs(title = "Forecast Lag 3", x = "Min Temp Error (F)", y = "") + 
  theme(axis.text.y = element_blank(), plot.margin=unit(c(1, -0.10, 1, -0.10), "cm"),
        axis.text = element_text(size = size1),
        axis.title = element_text(size = size2),
        plot.title = element_text(size = size3))

lagP1.l5 <- ggplot(lag5, aes(x = mnT_mean_abs, y = BSS)) + 
  geom_point(color = lag5$color, alpha = lag5$alphaP1, size = lag5$sizeP1) +
  theme_bw() +
  coord_fixed(ratio = 2.8, xlim = c(2, 8), ylim = c(0.4, 2.7), expand = TRUE) +
  scale_x_continuous(breaks = c(2:8)) +
  scale_y_continuous(breaks = c(seq(0.5, 2.5, 0.5))) +
  labs(title = "Forecast Lag 5", x = "", y = "Precip (1-BSS)") +
  theme(plot.margin=unit(c(1, -0.10, 1, 1), "cm"),
        axis.text = element_text(size = size1),
        axis.title = element_text(size = size2),
        plot.title = element_text(size = size3))

# grid.arrange(lagP1.l5, lagP1.l3, lagP1.l1, ncol = 3)
pdf("../images/Scatterplots/LagP1.pdf", height = 5, width = 11)
grid.draw(cbind(ggplotGrob(lagP1.l5), ggplotGrob(lagP1.l3), ggplotGrob(lagP1.l1), size = "last"))
dev.off()

# Plots showing bad precips
lagP2.l1 <- ggplot(lag1, aes(x = mxT_mean_abs, y = BSS)) + 
  geom_point(color = lag1$color, alpha = lag1$alphaP2, size = lag1$sizeP2) +
  theme_bw() +
  coord_fixed(ratio = 2.6, xlim = c(1.25, 7), ylim = c(0.4, 2.7), expand = TRUE) +
  scale_x_continuous(breaks = c(1:7)) +
  scale_y_continuous(breaks = c(seq(0.5, 2.5, 0.5))) +
  labs(title = "Forecast Lag 1", x = "", y = "") + 
  theme(axis.text.y = element_blank(), plot.margin=unit(c(1, 1, 1, -0.10), "cm"),
        axis.text = element_text(size = size1),
        axis.title = element_text(size = size2),
        plot.title = element_text(size = size3))

lagP2.l3 <- ggplot(lag3, aes(x = mxT_mean_abs, y = BSS)) + 
  geom_point(color = lag3$color, alpha = lag3$alphaP2, size = lag3$sizeP2) +
  theme_bw() +
  coord_fixed(ratio = 2.6, xlim = c(1.25, 7), ylim = c(0.4, 2.7), expand = TRUE) +
  scale_x_continuous(breaks = c(1:7)) +
  scale_y_continuous(breaks = c(seq(0.5, 2.5, 0.5))) +
  labs(title = "Forecast Lag 3", x = "Max Temp Error (F)", y = "") + 
  theme(axis.text.y = element_blank(), plot.margin=unit(c(1, -0.10, 1, -0.10), "cm"),
        axis.text = element_text(size = size1),
        axis.title = element_text(size = size2),
        plot.title = element_text(size = size3))

lagP2.l5 <- ggplot(lag5, aes(x = mxT_mean_abs, y = BSS)) + 
  geom_point(color = lag5$color, alpha = lag5$alphaP2, size = lag5$sizeP2) +
  theme_bw() +
  coord_fixed(ratio = 2.6, xlim = c(1.25, 7), ylim = c(0.4, 2.7), expand = TRUE) +
  scale_x_continuous(breaks = c(1:7)) +
  scale_y_continuous(breaks = c(seq(0.5, 2.5, 0.5))) +
  labs(title = "Forecast Lag 5", x = "", y = "Precip (1-BSS)") +
  theme(plot.margin=unit(c(1, -0.10, 1, 1), "cm"),
        axis.text = element_text(size = size1),
        axis.title = element_text(size = size2),
        plot.title = element_text(size = size3))

# grid.arrange(lagP2.l5, lagP2.l3, lagP2.l1, ncol = 3)
pdf("../images/Scatterplots/LagP2.pdf", height = 5, width = 11)
grid.draw(cbind(ggplotGrob(lagP2.l5), ggplotGrob(lagP2.l3), ggplotGrob(lagP2.l1), size = "last"))
dev.off()


# Plots showing outliers
lagP3.l1 <- ggplot(lag1, aes(x = mnT_mean_abs, y = mxT_mean_abs)) + 
  geom_point(color = lag1$color, alpha = lag1$alphaP3, size = lag1$sizeP3) +
  theme_bw() +
  coord_fixed(ratio = 1.1, xlim = c(2, 8), ylim = c(1.25, 7), expand = TRUE) +
  scale_x_continuous(breaks = c(2:8)) +
  scale_y_continuous(breaks = c(1:7)) +
  labs(title = "Forecast Lag 1", x = "", y = "") + 
  theme(axis.text.y = element_blank(), plot.margin=unit(c(1, 1, 1, -0.10), "cm"),
        axis.text = element_text(size = size1),
        axis.title = element_text(size = size2),
        plot.title = element_text(size = size3))

lagP3.l3 <- ggplot(lag3, aes(x = mnT_mean_abs, y = mxT_mean_abs)) + 
  geom_point(color = lag3$color, alpha = lag3$alphaP3, size = lag3$sizeP3) +
  theme_bw() +
  coord_fixed(ratio = 1.1, xlim = c(2, 8), ylim = c(1.25, 7), expand = TRUE) +
  scale_x_continuous(breaks = c(2:8)) +
  scale_y_continuous(breaks = c(1:7)) +
  labs(title = "Forecast Lag 3", x = "Min Temp Error (F)", y = "") + 
  theme(axis.text.y = element_blank(), plot.margin=unit(c(1, -0.10, 1, -0.10), "cm"),
        axis.text = element_text(size = size1),
        axis.title = element_text(size = size2),
        plot.title = element_text(size = size3))

lagP3.l5 <- ggplot(lag5, aes(x = mnT_mean_abs, y = mxT_mean_abs)) + 
  geom_point(color = lag5$color, alpha = lag5$alphaP3, size = lag5$sizeP3) +
  theme_bw() +
  coord_fixed(ratio = 1.1, xlim = c(2, 8), ylim = c(1.25, 7), expand = TRUE) +
  scale_x_continuous(breaks = c(2:8)) +
  scale_y_continuous(breaks = c(1:7)) +
  labs(title = "Forecast Lag 5", x = "", y = "Max Temp Error (F)") +
  theme(plot.margin=unit(c(1, -0.10, 1, 1), "cm"),
        axis.text = element_text(size = size1),
        axis.title = element_text(size = size2),
        plot.title = element_text(size = size3))

# Plots for slides
lag1.sub <- lag1[is.element(as.character(lag1$AirPtCd), as.character(s4$AirPtCd)), ]
lag3.sub <- lag3[is.element(as.character(lag3$AirPtCd), as.character(s4$AirPtCd)), ]
lag5.sub <- lag5[is.element(as.character(lag5$AirPtCd), as.character(s4$AirPtCd)), ]

lagPA.l1 <- ggplot(lag1, aes(x = mnT_mean_abs, y = BSS)) + 
  geom_point(color = lag1$color, alpha = 0.6, size = 1) +
  geom_point(data = lag1.sub, fill = lag1.sub$color, alpha = 1, size = 4, shape = 21) +
  theme_bw() +
  coord_fixed(ratio = 2.8, xlim = c(2, 8), ylim = c(0.4, 2.7), expand = TRUE) +
  scale_x_continuous(breaks = c(2:8)) +
  scale_y_continuous(breaks = c(seq(0.5, 2.5, 0.5))) +
  labs(title = "Forecast Lag 1", x = "", y = "") + 
  theme(axis.text.y = element_blank(), plot.margin=unit(c(1, 1, 1, -0.10), "cm"),
        axis.text = element_text(size = size1),
        axis.title = element_text(size = size1),
        plot.title = element_text(size = size2))

lagPA.l3 <- ggplot(lag3, aes(x = mnT_mean_abs, y = BSS)) + 
  geom_point(color = lag3$color, alpha = 0.6, size = 1) +
  geom_point(data = lag3.sub, fill = lag3.sub$color, alpha = 1, size = 4, shape = 21) +
  theme_bw() +
  coord_fixed(ratio = 2.8, xlim = c(2, 8), ylim = c(0.4, 2.7), expand = TRUE) +
  scale_x_continuous(breaks = c(2:8)) +
  scale_y_continuous(breaks = c(seq(0.5, 2.5, 0.5))) +
  labs(title = "Forecast Lag 3", x = "Min Temp Error (F)", y = "") + 
  theme(axis.text.y = element_blank(), plot.margin=unit(c(1, -0.10, 1, -0.10), "cm"),
        axis.text = element_text(size = size1),
        axis.title = element_text(size = size1),
        plot.title = element_text(size = size2))

lagPA.l5 <- ggplot(lag5, aes(x = mnT_mean_abs, y = BSS)) + 
  geom_point(color = lag5$color, alpha = 0.6, size = 1) +
  geom_point(data = lag5.sub, fill = lag5.sub$color, alpha = 1, size = 4, shape = 21) +
  theme_bw() +
  coord_fixed(ratio = 2.8, xlim = c(2, 8), ylim = c(0.4, 2.7), expand = TRUE) +
  scale_x_continuous(breaks = c(2:8)) +
  scale_y_continuous(breaks = c(seq(0.5, 2.5, 0.5))) +
  labs(title = "Forecast Lag 5", x = "", y = "Precip (1 - BSS)") +
  theme(plot.margin=unit(c(1, -0.10, 1, 1), "cm"),
        axis.text = element_text(size = size1),
        axis.title = element_text(size = size1),
        plot.title = element_text(size = size2))

dummy <- ggplot(lag5, aes(x = mnT_mean_abs, y = mxT_mean_abs)) + 
  geom_blank() +
  coord_fixed(ratio = 1.1, xlim = c(2, 8), ylim = c(1.25, 7), expand = TRUE) +
  # theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_line(colour = 'white', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'white', size=0.5, linetype='solid'),
        axis.title = element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        panel.grid.major = element_blank(), 
        # axis.line = element_line(colour = "white"),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"))
dummy1 <- ggplot(lag5, aes(x = mnT_mean_abs, y = mxT_mean_abs)) + 
  geom_blank() +
  coord_fixed(ratio = 1.1, xlim = c(2, 8), ylim = c(1.25, 7), expand = TRUE) +
  # theme_minimal() +
  annotate("text", x = 5, y = 4.125, label = "(a)", size = 15) + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_line(colour = 'white', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'white', size=0.5, linetype='solid'),
        axis.title = element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        panel.grid.major = element_blank(), 
        # axis.line = element_line(colour = "white"),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"))
dummy2 <- ggplot(lag5, aes(x = mnT_mean_abs, y = mxT_mean_abs)) + 
  geom_blank() +
  coord_fixed(ratio = 1.1, xlim = c(2, 8), ylim = c(1.25, 7), expand = TRUE) +
  # theme_minimal() +
  annotate("text", x = 5, y = 4.125, label = "(b)", size = 15) + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_line(colour = 'white', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'white', size=0.5, linetype='solid'),
        axis.title = element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        panel.grid.major = element_blank(), 
        # axis.line = element_line(colour = "white"),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"))
dummy3 <- ggplot(lag5, aes(x = mnT_mean_abs, y = mxT_mean_abs)) + 
  geom_blank() +
  coord_fixed(ratio = 1.1, xlim = c(2, 8), ylim = c(1.25, 7), expand = TRUE) +
  # theme_minimal() +
  annotate("text", x = 5, y = 4.125, label = "(c)", size = 15) + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_line(colour = 'white', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'white', size=0.5, linetype='solid'),
        axis.title = element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        panel.grid.major = element_blank(), 
        # axis.line = element_line(colour = "white"),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"))

# grid.arrange(lagP3.l5, lagP3.l3, lagP3.l1, ncol = 3)
pdf("../images/Scatterplots/LagP3.pdf", height = 5, width = 11)
grid.draw(cbind(ggplotGrob(lagP3.l5), ggplotGrob(lagP3.l3), ggplotGrob(lagP3.l1), size = "last"))
dev.off()


#===========================================================================
# Final graphs
#===========================================================================

pdf("../images/final/ScatterTop1.pdf", height = 4, width = 11)
grid.draw(cbind(ggplotGrob(lagP1.l5), ggplotGrob(lagP1.l3), ggplotGrob(lagP1.l1), 
                ggplotGrob(dummy), size = "first"))
dev.off()

pdf("../images/final/ScatterBottom1.pdf", height = 5, width = 11)
grid.arrange(g1, g4, widths = c(2, 4), ncol = 2)
dev.off()

pdf("../images/final/ScatterTop2.pdf", height = 4, width = 11)
grid.draw(cbind(ggplotGrob(lagP2.l5), ggplotGrob(lagP2.l3), ggplotGrob(lagP2.l1), 
                ggplotGrob(dummy), size = "first"))
dev.off()

pdf("../images/final/ScatterBottom2.pdf", height = 5, width = 11)
grid.arrange(g2, g5, widths = c(2, 4), ncol = 2)
dev.off()

pdf("../images/final/ScatterTop3.pdf", height = 4, width = 11)
grid.draw(cbind(ggplotGrob(lagP3.l5), ggplotGrob(lagP3.l3), ggplotGrob(lagP3.l1), 
                ggplotGrob(dummy), size = "first"))
dev.off()

pdf("../images/final/ScatterBottom3.pdf", height = 5, width = 11)
grid.arrange(g3, g6, widths = c(2, 4), ncol = 2)
dev.off()

pdf("../images/final/ScatterTopSlides.pdf", height = 4, width = 11)
grid.draw(cbind(ggplotGrob(lagPA.l5), ggplotGrob(lagPA.l3), ggplotGrob(lagPA.l1), 
                ggplotGrob(dummy), size = "first"))
dev.off()

pdf("../images/final/ScatterBottomSlides.pdf", height = 5, width = 11)
grid.arrange(gA, gB, widths = c(2, 4), ncol = 2)
dev.off()

#===========================================================================
# Final paper graphs
#===========================================================================

pdf("../images/final/paper/ScatterTop1.pdf", height = 4, width = 11)
grid.draw(cbind(ggplotGrob(lagP1.l5), ggplotGrob(lagP1.l3), ggplotGrob(lagP1.l1), 
                ggplotGrob(dummy1), size = "first"))
dev.off()

pdf("../images/final/paper/ScatterBottom1.pdf", height = 5, width = 11)
grid.arrange(g1, g4, widths = c(2, 4), ncol = 2)
dev.off()

pdf("../images/final/paper/ScatterTop2.pdf", height = 4, width = 11)
grid.draw(cbind(ggplotGrob(lagP2.l5), ggplotGrob(lagP2.l3), ggplotGrob(lagP2.l1), 
                ggplotGrob(dummy2), size = "first"))
dev.off()

pdf("../images/final/paper/ScatterBottom2.pdf", height = 5, width = 11)
grid.arrange(g2, g5, widths = c(2, 4), ncol = 2)
dev.off()

pdf("../images/final/paper/ScatterTop3.pdf", height = 4, width = 11)
grid.draw(cbind(ggplotGrob(lagP3.l5), ggplotGrob(lagP3.l3), ggplotGrob(lagP3.l1), 
                ggplotGrob(dummy3), size = "first"))
dev.off()

pdf("../images/final/paper/ScatterBottom3.pdf", height = 5, width = 11)
grid.arrange(g3, g6, widths = c(2, 4), ncol = 2)
dev.off()


#=============================================================================
# First, create a set of boxes that we can color differently. 
tbox.x <- c(0, 0, 5, 5, 0)
tbox.y <- c(0, 1, 1, 0, 0)

# Now create a series of 6 boxes. 
boxes <- data.frame(x = c(tbox.x, tbox.x+5, tbox.x+10, tbox.x+15, 
                          tbox.x+20, tbox.x+25),
                    y = rep(tbox.y, 6),
                    group = c(rep(1, 5), rep(2, 5), rep(3, 5), 
                              rep(4, 5), rep(5, 5), rep(6, 5)))

legend <- ggplot(boxes, aes(x = x, y = y)) + 
  geom_polygon(aes(fill = factor(group)), color = "white") + 
  coord_fixed() +
  scale_color_manual(values = c("#f781bf", "#4daf4a", "#377eb8",  
                                "#e41a1c", "#984ea3", "#ff7f00")) + 
  scale_fill_manual(values = c("#f781bf", "#4daf4a", "#377eb8", 
                               "#e41a1c", "#984ea3",  "#ff7f00")) + 
  annotate("text", x = 2.5 + c(0, 5, 10, 15, 20, 25), 
           y = rep(0.5, 6), 
           label = c("Cali-Florida", "Southeast", "Northeast", 
                     "Intermountain West", "Midwest", "Southwest"), 
           color = "white", size = c(5.5, 5.5, 5.5, 3.5, 5.5, 5.5)) + 
  theme(legend.position = "none",
        panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())

pdf("../images/final/paper/ScatterLegend.pdf", height = 1, width = 11)
legend
dev.off()
#=============================================================================



# plot_grid(lagP3.l5, lagP3.l3, lagP3.l1, align = 'v', ncol = 3)
# 
# ggarrange(lagP3.l5$plot, lagP3.l5$plot, lagP3.l5$plot, heights = c(2, 0.7),
#           ncol = 3, nrow = 1, align = "v")
# 
# p1 <- ggplot_gtable(ggplot_build(lagP3.l1))
# p3 <- ggplot_gtable(ggplot_build(lagP3.l3))
# p5 <- ggplot_gtable(ggplot_build(lagP3.l5))
# 
# maxWidth = unit.pmax(p1$widths[2:3], p3$widths[2:3], p5$widths[2:3])
# p1$widths[2:3] <- maxWidth
# p3$widths[2:3] <- maxWidth
# p5$widths[2:3] <- maxWidth
# 
# maxHeight = unit.pmax(p1$heights[c(2, 7)], p3$heights[c(2, 7)], p5$heights[c(2, 7)])
# p1$heights[c(2, 7)] <- maxHeight
# p3$heights[c(2, 7)] <- maxHeight
# p5$heights[c(2, 7)] <- maxHeight
# 
# grid.arrange(p1, p3, p5, widths = c(2, 3), heights = c(2, 7))
# grid.arrange(p1, p3, p5, nrow = 1)
# 
# grid.arrange(p1, p3, p5, heights = c(3, 2), nrow = 1)
