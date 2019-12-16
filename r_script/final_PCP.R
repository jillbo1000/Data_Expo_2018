# Help on PCP plots from 
# https://www.r-bloggers.com/parallel-coordinate-plots-for-discrete-and-categorical-data-in-r-a-comparison/
library(GGally)
library(reshape2)

# Read in cluster variable file. 
library(dplyr)
library(lubridate)
library(tidyr)
library(cluster)
library(fpc)
library(ggplot2)
library(ggmap)
library(fiftystater)
library(maps)
library(RColorBrewer)
library(gridExtra)
library(grid)

cluster2 = read.csv("../Data/clusterMapFinal.csv")

cluster2$name <- paste(cluster2$city, cluster2$stateABB, sep = ", ")

clusterSum <- cluster2 %>% group_by(Cluster) %>% summarize(maxabsG = mean(mxT_mean_abs))
cluster2 <- left_join(cluster2, clusterSum, by = "Cluster") %>% arrange(maxabsG, mxT_mean_abs)
cluster2$maxabsG <- NULL

cluster2$AirPtCd <- factor(cluster2$AirPtCd, levels = cluster2$AirPtCd)
cluster2$Cluster <- factor(cluster2$Cluster, levels = unique(cluster2$Cluster))

cluster2 <- cluster2 %>% 
  dplyr::select(-mxT_sd, -mnT_sd, -mxT_sd_abs, -mnT_sd_abs,
                -mxT_mean, -mnT_mean, -mxT_mean_abs, -mnT_mean_abs,
                -BSS)

pal2 <- brewer.pal(8, "Set1")[c(1:5, 8)]
pal2 <- pal2[rev(c(1, 4, 2, 3, 6, 5))]

# Designate "names of interest"
noi <- c("Austin, NV", "Duluth, MN", "Watertown, NY", "Fresno, CA", 
         "Key West, FL")
arpts <- cluster2$AirPtCd[is.element(cluster2$name, noi)]

cluster2.sd <- cluster2 %>% 
  dplyr::select(contains("Sd"), elevation, dist2coast, Cluster, AirPtCd)
cluster2.sd[, 1:15] <- scale(cluster2.sd[, 1:15])

cluster2.other <- cluster2 %>% 
  dplyr::select(-contains("Sd"), -city, -state, -longitude, 
                -latitude, -stateABB, -name)
cluster2.other[, 2:16] <- scale(cluster2.other[, 2:16])

cluster2.sd.long <- melt(cluster2.sd, id.vars = c("Cluster", "AirPtCd"))
cluster2.sd.long$value[is.element(cluster2.sd.long$variable, c("elevation", "dist2coast"))] <- 0
cluster2.other.long <- melt(cluster2.other, id.vars = c("Cluster", "AirPtCd"))

# Emphasize cluster
cluster2.sd.long.sub <- cluster2.sd.long[is.element(as.character(cluster2.sd.long$Cluster), "6"), ]
cluster2.other.long.sub <- cluster2.other.long[is.element(as.character(cluster2.other.long$Cluster), "6"), ]

# Emphasize station
# cluster2.sd.long.sub <- cluster2.sd.long[is.element(as.character(cluster2.sd.long$AirPtCd), "KSFO"), ]
# cluster2.other.long.sub <- cluster2.other.long[is.element(as.character(cluster2.other.long$AirPtCd), "KSFO"), ]


# Manually jitter values to fit. 
suby <- cluster2.sd.long.sub$value[cluster2.sd.long.sub$variable == "Sd_Max_Temp"]
suby[3] <- suby[3] + 0.2
suby[5] <- suby[5] - 0.2

cluster2.sd.long$title1 <- "Measurement Standard Deviations"
# Angle label help:
# - https://stackoverflow.com/questions/1330989/rotating-and-spacing-axis-labels-in-ggplot2 
p1 <- ggplot(cluster2.sd.long, aes(x = variable, y = value, group = factor(AirPtCd))) + 
  geom_path(aes(color = factor(Cluster)), alpha = 0.4) +
  scale_color_manual(values = pal2) + 
  ylim(-5.05, 5) +
  ylab( "Z") +
  geom_path(data = cluster2.sd.long.sub,
            aes(x = variable, y = value, group = factor(AirPtCd), color = factor(Cluster)), 
            lwd = 1.5, alpha = 0.6) +
  # annotate("text", x = 0, y = suby, 
  #          label = na.omit(cluster2$name[is.element(cluster2.sd.long$AirPtCd, arpts)]),
  #          col = na.omit(pal2[cluster2$Cluster[is.element(cluster2.sd.long$AirPtCd, arpts)]])) + 
  # expand_limits(x = -.9) + 
  facet_grid(~title1) + 
  theme_bw() +
  scale_x_discrete(labels=c("Max Temp", "Min Temp", "Precip", "Max DP", "Min DP", 
                            "Max Hum", "Min Hum", "Max SLP", "Min SLP", "Vis", 
                            "Clouds", "Mean WS", "Max WS", "Elevation", 
                            "Dist2Coast")) + 
  theme(legend.position = "none",
        axis.text.y = element_text(size = 18),
        axis.text.x = element_text(angle = 20, hjust = 1, vjust = 1, size = 22),
        #axis.ticks.x = element_blank(),
        axis.title.x = element_blank(), 
        axis.title.y = element_text(size = 18),
        strip.text.x = element_text(size = 24))

# Expand idea from scale_x_discrete help file. 
cluster2.other.long$title1 <- "Measurement Means"
p2 <- ggplot(cluster2.other.long, aes(x = variable, y = value, group = factor(AirPtCd))) + 
  geom_path(aes(color = factor(Cluster)), alpha = 0.3) +
  scale_color_manual(values = pal2) + 
  ylim(-5.05, 5) +
  ylab( "Z") +
  geom_path(data = cluster2.other.long.sub,
            aes(x = variable, y = value, group = factor(AirPtCd), color = factor(Cluster)), 
            lwd = 1.5, alpha = 0.6) + 
  # annotate("text", x = 0, y = cluster2.other.long.sub$value[cluster2.other.long.sub$variable == "Max_Temp"], 
  #          label = na.omit(cluster2$name[is.element(cluster2.sd.long$AirPtCd, arpts)]),
  #         col = na.omit(pal2[cluster2$Cluster[is.element(cluster2.sd.long$AirPtCd, arpts)]])) + 
  # expand_limits(x = -.9) + 
  facet_grid(~title1) + 
  theme_bw() +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 18),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(), 
        axis.title.y = element_text(size = 18),
        strip.text.x = element_text(size = 24))

pdf("../images/final/finalPCP.pdf", width =  20, height = 6)
# Grid.draw solution:
# - https://gist.github.com/tomhopper/faa24797bb44addeba79
grid.draw(rbind(ggplotGrob(p2), ggplotGrob(p1), size = "last"))
dev.off()

pdf("../images/final/finalPCP_Slides.pdf", width =  20, height = 7)
# Grid.draw solution:
# - https://gist.github.com/tomhopper/faa24797bb44addeba79
grid.draw(rbind(ggplotGrob(p2), ggplotGrob(p1), size = "last"))
dev.off()

png("../images/pcptest2.png", width =  14, height = 7, units = "in", res = 300)
# Grid.draw solution:
# - https://gist.github.com/tomhopper/faa24797bb44addeba79
grid.draw(rbind(ggplotGrob(p2), ggplotGrob(p1), size = "last"))
dev.off()


pdf("../images/pcptest2.pdf", width =  20, height = 5)
# Grid.draw solution:
# - https://gist.github.com/tomhopper/faa24797bb44addeba79
grid.draw(rbind(ggplotGrob(p2), ggplotGrob(p1), size = "last"))
dev.off()


#=============================================================================
# First, create a set of boxes that we can color differently. 
tbox.x <- c(0, 0, 5, 5, 0)
tbox.y <- c(0, 6, 6, 0, 0)

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
           y = rep(3, 6), 
           label = c("Cali-Florida", "Southeast", "Northeast", 
                     "Intermountain West", "Midwest", "Southwest"), 
           color = "white", size = c(7.5, 7.5, 7.5, 7.5, 7.5, 7.5)) + 
  theme(legend.position = "none",
        panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())
#=============================================================================

pdf("../images/final/paper/finalPCP.pdf", width =  20, height = 8)
# Grid.draw solution:
# - https://gist.github.com/tomhopper/faa24797bb44addeba79
grid.draw(rbind(rbind(ggplotGrob(p2), ggplotGrob(p1), size = "last"), ggplotGrob(legend), size = "first"))
dev.off()
