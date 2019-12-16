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

cluster2.sd.long.sub <- cluster2.sd.long[is.element(cluster2.sd.long$AirPtCd, arpts), ]
cluster2.other.long.sub <- cluster2.other.long[is.element(cluster2.sd.long$AirPtCd, arpts), ]

# Manually jitter values to fit. 
suby <- cluster2.sd.long.sub$value[cluster2.sd.long.sub$variable == "Sd_Max_Temp"]
suby[3] <- suby[3] + 0.2
suby[5] <- suby[5] - 0.2

cluster2.sd.long$title1 <- "Measurement Standard Deviations"
# Angle label help:
# - https://stackoverflow.com/questions/1330989/rotating-and-spacing-axis-labels-in-ggplot2 
p1 <- ggplot(cluster2.sd.long, aes(x = variable, y = value, group = factor(AirPtCd))) + 
  geom_path(aes(color = factor(Cluster)), alpha = 0.3) +
  scale_color_manual(values = pal2) + 
  geom_path(data = cluster2.sd.long.sub,
            aes(x = variable, y = value, group = factor(AirPtCd), color = factor(Cluster)), 
            lwd = 1.5, alpha = 0.6) +
  annotate("text", x = 0, y = suby, 
           label = na.omit(cluster2$name[is.element(cluster2.sd.long$AirPtCd, arpts)]),
           col = na.omit(pal2[cluster2$Cluster[is.element(cluster2.sd.long$AirPtCd, arpts)]])) + 
  expand_limits(x = -.9) + 
  facet_grid(~title1) + 
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = -10, vjust  = 0.5),
        axis.title = element_blank())

# Expand idea from scale_x_discrete help file. 
cluster2.other.long$title1 <- "Measurement Means"
p2 <- ggplot(cluster2.other.long, aes(x = variable, y = value, group = factor(AirPtCd))) + 
  geom_path(aes(color = factor(Cluster)), alpha = 0.3) +
  scale_color_manual(values = pal2) + 
  geom_path(data = cluster2.other.long.sub,
            aes(x = variable, y = value, group = factor(AirPtCd), color = factor(Cluster)), 
            lwd = 1.5, alpha = 0.6) + 
  annotate("text", x = 0, y = cluster2.other.long.sub$value[cluster2.other.long.sub$variable == "Max_Temp"], 
           label = na.omit(cluster2$name[is.element(cluster2.sd.long$AirPtCd, arpts)]),
          col = na.omit(pal2[cluster2$Cluster[is.element(cluster2.sd.long$AirPtCd, arpts)]])) + 
  expand_limits(x = -.9) + 
  facet_grid(~title1) + 
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title = element_blank())

png("../images/pcptest.png", width =  14, height = 7, units = "in", res = 300)
# Grid.draw solution:
# - https://gist.github.com/tomhopper/faa24797bb44addeba79
grid.draw(rbind(ggplotGrob(p2), ggplotGrob(p1), size = "last"))
dev.off()


pdf("../images/pcptest.pdf", width =  20, height = 5)
# Grid.draw solution:
# - https://gist.github.com/tomhopper/faa24797bb44addeba79
grid.draw(rbind(ggplotGrob(p2), ggplotGrob(p1), size = "last"))
dev.off()

