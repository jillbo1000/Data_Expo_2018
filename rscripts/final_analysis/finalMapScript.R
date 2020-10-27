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

cluster2 <- read.csv("../../data/summary_city.csv")
cluster <- cluster2

# Impute Baltimore values with nearest neighbor (Dover, DE). 
cluster2$Min_Vis[cluster2$AirPtCd == "KDMH"] <- cluster2$Min_Vis[cluster2$AirPtCd == "KDOV"]
cluster2$Sd_Vis[cluster2$AirPtCd == "KDMH"] <- cluster2$Sd_Vis[cluster2$AirPtCd == "KDOV"]
cluster2$CloudCover[cluster2$AirPtCd == "KDMH"] <- cluster2$CloudCover[cluster2$AirPtCd == "KDOV"]
cluster2$Sd_CloudCover[cluster2$AirPtCd == "KDMH"] <- cluster2$Sd_CloudCover[cluster2$AirPtCd == "KDOV"]

# Impute Austin, NV values with nearest neighbor (Reno)
cluster2$CloudCover[cluster2$AirPtCd == "KP68"] <- cluster2$CloudCover[cluster2$AirPtCd == "KRNO"]
cluster2$Sd_CloudCover[cluster2$AirPtCd == "KP68"] <- cluster2$Sd_CloudCover[cluster2$AirPtCd == "KRNO"]
cluster2$Min_Vis[cluster2$AirPtCd == "KP68"] <- cluster2$Min_Vis[cluster2$AirPtCd == "KRNO"]
cluster2$Sd_Vis[cluster2$AirPtCd == "KP68"] <- cluster2$Sd_Vis[cluster2$AirPtCd == "KRNO"]

cc2 <- cluster2 %>% dplyr::select(-AirPtCd, -city, -state, -longitude, -latitude,
                                  -mxT_sd, -mxT_mean, -mnT_sd, -mnT_mean,
                                  -mxT_sd_abs, -mxT_mean_abs, -mnT_sd_abs, -mnT_mean_abs,
                                  -BSS)



cc2 <- scale(cc2)

d2 <- dist(cc2, method = "euclidean")
c2 <- hclust(d2, method = "ward.D2")
l2 <- cutree(c2, k = 6)

cluster2$Cluster <- l2

tmp <- as.data.frame(cluster2)
tmp$state <- as.character(tmp$state)
# Help for abbreviations from:
# - https://stackoverflow.com/questions/5411979/state-name-to-abbreviation-in-r
tmp$stateABB <- "none"
for(i in 1:nrow(tmp)){tmp$stateABB[i] <- state.abb[grep(tmp$state[i], state.name)]}
cluster2$stateABB <- tmp$stateABB
tmp$state <- tolower(tmp$state)
longitude <- c(-117.25, -108.50)
latitude <- c(27.2, 27)

pal1 <- brewer.pal(6, "Dark2")
pal2 <- brewer.pal(8, "Set1")[c(1:5, 8)]
pal2 <- pal2[c(4, 2, 3, 6, 1, 5)]
pal3 <- brewer.pal(7, "Set3")[-2]

# Create copy of tmp file where we adjust the coordinates for alaska and hawaii
tmp2 <- tmp
tmp2$latitude[tmp2$state == "alaska"] <- latitude[1]
tmp2$latitude[tmp2$state == "hawaii"] <- latitude[2]
tmp2$longitude[tmp2$state == "alaska"] <- longitude[1]
tmp2$longitude[tmp2$state == "hawaii"] <- longitude[2]

noi <- c("Austin, NV", "Duluth, MN", "Watertown, NY", "Fresno, CA", "Key West, FL")
tmp2$name <- paste(tmp2$city, tmp2$stateABB, sep = ", ")
tmp2.sub <- tmp2[is.element(tmp2$name, noi), ]

png("../../images/test/mapTrial1.png", width = 14, height = 10, units = "in", res = 300)
p2 <- ggplot(tmp2, aes(map_id = state)) +
  # map points to the fifty_states shape data
  geom_map(fill = "gray90", color = "gray80", map = fifty_states) +
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme(legend.position = "bottom",
        panel.background = element_blank()) +
  geom_point(data = tmp2, aes(x = longitude, y = latitude),
             fill = pal2[tmp2$Cluster], size = 3, shape = I(21)) + 
  geom_point(data = tmp2.sub, aes(x = longitude, y = latitude),
             fill = pal2[tmp2.sub$Cluster], size = 6, shape = I(21)) 

p2
dev.off()

pdf("../../images/final/finalMap.pdf", width = 14, height = 10)
p2 <- ggplot(tmp2, aes(map_id = state)) +
  # map points to the fifty_states shape data
  geom_map(fill = alpha("gray80", 0.5), color = alpha("gray60", 0.5), map = fifty_states) +
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme(legend.position = "bottom",
        panel.background = element_blank()) +
  geom_point(data = tmp2, aes(x = longitude, y = latitude),
             fill = alpha(pal2[tmp2$Cluster], 0.8), size = 6, shape = I(21))
p2
dev.off()

pdf("../../images/final/poster/finalMap_Slides.pdf", width = 14, height = 10)
p3 <- ggplot(tmp2, aes(map_id = state)) +
  # map points to the fifty_states shape data
  # geom_map(fill = "gray90", color = "gray80", map = fifty_states) +
  geom_map(fill = alpha("gray80", 0.5), color = alpha("gray60", 0.5), map = fifty_states) +
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme(legend.position = "bottom",
        panel.background = element_blank()) +
  geom_point(data = tmp2, aes(x = longitude, y = latitude),
             fill = alpha(pal2[tmp2$Cluster], 0.8), size = 6, shape = I(21))
p3
dev.off()



# Add the legend.
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
           color = "white", size = c(7, 7, 7, 5, 7, 7)) + 
  theme(legend.position = "none",
        panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())
#=============================================================================

pdf("../../images/final/paper/finalMap.pdf", width = 14, height = 10)
grid.arrange(grobs = list(ggplotGrob(p2), ggplotGrob(legend)), heights = c(.9, .1))
dev.off()

# Save the map cluster results as a new data frame as we will use this 
# same data set often. 
#write.csv(cluster2, file = "../../data/clusterMapFinal.csv",
#          row.names = FALSE, quote = FALSE)

