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

cluster2 <- read.csv("../../data/summary_city.csv")
cluster2$state <- as.character(cluster2$state)
# Help for abbreviations from:
# - https://stackoverflow.com/questions/5411979/state-name-to-abbreviation-in-r
cluster2$stateABB <- "none"
for(i in 1:nrow(cluster2)){cluster2$stateABB[i] <- state.abb[grep(cluster2$state[i], state.name)]}
cluster2$name <- paste(cluster2$city, cluster2$stateABB, sep = ", ")

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

cc2 <- cluster2 %>% dplyr::select(-AirPtCd, -city, -state, -stateABB, -name, -longitude, -latitude,
                                  -mxT_sd, -mxT_mean, -mnT_sd, -mnT_mean,
                                  -mxT_sd_abs, -mxT_mean_abs, -mnT_sd_abs, -mnT_mean_abs,
                                  -BSS)

cc2 <- scale(cc2)

d2 <- dist(cc2, method = "euclidean")
c2 <- hclust(d2, method = "ward.D2")

cluster2$Cluster5 <- cutree(c2, k = 5)
cluster2$Cluster6 <- cutree(c2, k = 6)
cluster2$Cluster7 <- cutree(c2, k = 7)
cluster2$Cluster8 <- cutree(c2, k = 8)

cluster2 <- cluster2 %>% dplyr::select(AirPtCd, Cluster5, Cluster6, Cluster7, Cluster8)

write.csv(cluster2, "../../data/cluster_summary.csv", row.names = FALSE)

# Add locations to locationsFinal_preclust.csv
locs <- read.csv("../../data/locationsFinal_preclust.csv")
locs <- left_join(locs, cluster2[, c(1, 3)])

col <- brewer.pal(8, "Set1")
col <- col[c(4, 2, 3, 8, 1, 5)]

locs$color <- col[locs$Cluster6]

locs$name <- paste(locs$city, locs$state, sep = ", ")

clust.name <- c("Midwest", "Northeast", "Southeast", "Cali-Florida", "Intermountain West", "Southwest")
locs$ClustName <- clust.name[locs$Cluster6]

# Convert to english units
locs$elevation <- (locs$elevation)*3.28084
locs$dist2coast <- locs$dist2coast*0.621371

write.csv(locs, file = "../../data/locationsFinal.csv", row.names = FALSE)



