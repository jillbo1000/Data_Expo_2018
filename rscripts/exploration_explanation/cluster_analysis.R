# Set working directory to source file location prior to running. 

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

# The following source file contains several functions that help with 
# doing a lot of cluster analysis. The file includes the following functions
# fastClust: Creates a list of hclust objects and a pdf that contains
#            all of the dendrograms
#
source("cluster_functions.R")

###############################################################################
#
#                         Reduced Cluster Analysis_Bean Version
#
###############################################################################
# Perform the clustering with a subset of variables, most notably excluding 
# longitude and latitude. 

cluster2 <- read.csv("../../data/summary_city.csv")
cluster <- cluster2
colnames(cluster2)
summary(cluster2)

# Check for missing values
apply(cluster2, 2, function(x) sum(is.na(x)))

cluster2[cluster2$AirPtCd == "KDMH", ]
cluster2[cluster2$AirPtCd == "KP68", ]

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


apply(cluster2, 2, function(x) sum(is.na(x))) # All missing values taken care of. 

# Subset the results of the cluster and scale the variables.
cc2 <- cluster2 %>% dplyr::select(-AirPtCd, -city, -state, -longitude, -latitude,
                                  -mxT_sd, -mxT_mean, -mnT_sd, -mnT_mean,
                                  -mxT_sd_abs, -mxT_mean_abs, -mnT_sd_abs, -mnT_mean_abs,
                                  -BSS)

cc2 <- scale(cc2)

#===================================================================================
#     Dendrograms for each of the distance and clustering types
#===================================================================================

euc <- fastClust(cc2, distType = "euclidean", graphName = "../../images/cluster/Dendro_Euc_Scaled.pdf")
fastClust(cc2, distType = "maximum", graphName = "../../images/cluster/Dendro_Max_Scaled2.pdf")
fastClust(cc2, distType = "manhattan", graphName = "../../images/cluster/Dendro_Man_Scaled2.pdf")
fastClust(cc2, distType = "canberra", graphName = "../../images/cluster/Dendro_Can_Scaled2.pdf")
fastClust(cc2, distType = "minkowski", graphName = "../../images/cluster/Dendro_Mink_Scaled2.pdf")


#===================================================================================
#     Determine the number of clusters for K-means using graphs and 
#     other metrics. The graphs are read like scree plots.
#===================================================================================

wss <- (nrow(cc2) - 1) * sum(apply(cc2, 2, var))
for(i in 2:30) wss[i] <- sum(kmeans(cc2, centers = i)$withinss)
plot(1:30, wss, type = "b", xlab = "Number of Clusters",
     ylab = "Within groups sum of squares")

wss <- (nrow(cc2) - 1) * sum(apply(cc2, 2, var))
for(i in 2:10) wss[i] <- sum(kmeans(cc2, centers = i)$withinss)
plot(1:10, wss, type = "b", xlab = "Number of Clusters",
     ylab = "Within groups sum of squares")

# K-Means Cluster Analysis using kmeans
fit <- kmeans(cc2, 8) # 8 cluster solution
# get cluster means 
aggregate(cc2, by = list(fit$cluster),FUN = mean)
# append cluster assignment
mydata <- data.frame(cc2, fit$cluster)

# The pamk function will give you an estimate of what k should be. 
# Every method chose 2 clusters, which is useless
pamk(cc2, krange = 2:12, criterion = "asw") # Only chooses 2 clusters as optimal
pamk(cc2, krange = 2:12, criterion = "multiasw") # Only chooses 2 clusters as optimal
pamk(cc2, krange = 2:12, criterion = "ch") # Only chooses 3 clusters as optimal


#===================================================================================
#     Make maps with the clusters. Each PDF has all of the pertinant
#     clustering methods. The most promising is Euclidean with Ward D2
#     with 6 clusters. 
#===================================================================================

pdf("../../images/cluster/K-means2.pdf", width = 11, height = 8.5)
clustPlot(cc2, trimDat = cluster2[, 38:40])
dev.off()

pdf("../../images/cluster/Euclidean2.pdf", width = 11, height = 8.5)
clustPlot(cc2, type = "euclidean",  trimDat = cluster2[, 38:40])
dev.off()

pdf("../../images/cluster/Maximum2.pdf", width = 11, height = 8.5)
clustPlot(cc2, type = "maximum", trimDat = cluster2[, 38:40])
dev.off()

pdf("../../images/cluster/Manhattan2.pdf", width = 11, height = 8.5)
clustPlot(cc2, type = "manhattan", trimDat = cluster2[, 38:40])
dev.off()

pdf("../../images/cluster/Canberra2.pdf", width = 11, height = 8.5)
clustPlot(cc2, type = "canberra", trimDat = cluster2[, 38:40])
dev.off()


#===================================================================================
#     Export the clusters into a dataset 
#===================================================================================

clust <- fastClust(cc2, distType = "euclidean")
clust.dat <- as.data.frame(cluster$AirPtCd)
clust.dat$Cluster5 <- cutree(clust[[2]], k = 5)
clust.dat$Cluster6 <- cutree(clust[[2]], k = 6)
clust.dat$Cluster7 <- cutree(clust[[2]], k = 7)
clust.dat$Cluster8 <- cutree(clust[[2]], k = 8)
colnames(clust.dat)[1] = "AirPtCd"

# write.csv(clust.dat, "../../data/cluster_summary.csv", row.names = FALSE)

