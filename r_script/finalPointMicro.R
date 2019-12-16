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

cluster2$AirPtCd <- factor(cluster2$AirPtCd, levels = cluster2$AirPtCd)
cluster2$Cluster <- factor(cluster2$Cluster, levels = unique(cluster2$Cluster))

pal2 <- brewer.pal(8, "Set1")[c(1:5, 8)]
pal2 <- pal2[rev(c(1, 4, 2, 3, 6, 5))]
  
# Designate "names of interest"
noi <- c("Austin, NV", "Duluth, MN", "Watertown, NY", "Fresno, CA", "Key West, FL")
a = ifelse(cluster2$name %in% noi, "black", "gray50")
b = ifelse(cluster2$name %in% noi, 2, 1)
c = ifelse(cluster2$name %in% noi, 11, 10)

cluster2$Size <- b

cluster2$title1 <- "Abs Max Temp Error" 
cluster2$title2 <- "Abs Min Temp Error" 
cluster2$title3 <- "-Brier Skill Score" 

  tempsALL <- ggplot(cluster2, aes(x = factor(name, levels = name), y = mxT_mean_abs)) +
    geom_point(aes(col = Cluster, size = factor(Size))) +
    scale_size_manual(values = c(1, 3)) + 
    scale_y_continuous(limits = c(1, 7), breaks = c(1, 3, 5, 7)) + 
    geom_line(aes(group = Cluster, col = Cluster)) + 
    scale_color_manual(values = pal2) +
    facet_grid(~title1) + 
    theme_bw() + 
    theme(legend.position = "none",
          axis.title.y = element_blank(),
          axis.text.y = element_text(color = a, size = c),
          axis.title.x = element_blank(),
          axis.text.x = element_text(size = 14),
          strip.text = element_text(size = 16)) + 
    coord_flip()
  
  tempsALL_min <- ggplot(cluster2, aes(x = factor(name, levels = name), y = mnT_mean_abs)) +
    geom_point(aes(col = Cluster, size = factor(Size))) +
    scale_size_manual(values = c(1, 3)) + 
    scale_y_continuous(limits = c(2, 8), breaks = c(2, 4, 6, 8)) + 
    geom_line(aes(group = Cluster, col = Cluster)) + 
    scale_color_manual(values = pal2) +
    facet_grid(~title2) + 
    theme_bw() + 
    theme(legend.position = "none",
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_text(size = 14),
          strip.text = element_text(size = 16)) + 
    coord_flip()
  
  precipALL <- ggplot(cluster2, aes(x = factor(name, levels = name), y = -BSS)) +
    geom_point(aes(col = Cluster, size = factor(Size))) +
    scale_size_manual(values = c(1, 3)) + 
    scale_y_continuous(limits = c(-.4, .81), breaks = c(-.4, 0, .4, .8)) + 
    geom_line(aes(group = Cluster, col = Cluster)) + 
    scale_color_manual(values = pal2) +
    facet_grid(~title3) + 
    theme_bw() + 
    theme(legend.position = "none",
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_text(size = 14),
          strip.text = element_text(size = 16)) + 
    coord_flip()

png("../images/finalPointMicro.png", height = 15, width = 10, 
    unit = "in", res = 300)
# Grid.draw solution:
# - https://gist.github.com/tomhopper/faa24797bb44addeba79
grid.draw(cbind(ggplotGrob(tempsALL), ggplotGrob(tempsALL_min), ggplotGrob(precipALL), size = "last"))
dev.off()

