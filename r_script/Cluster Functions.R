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

################ Functions for creating clusters

###############################################################################
#
#                           fastClust
#
###############################################################################

# The function fastClust will perform heirarchical clustering on a 
# given dataset. The user specifies the distance criteria. The choices for
# distance are: euclidean, maximum, manhattan, canberra, and minkowski.
# The user also specifies the names of the clusters. The default 
# is the state and city of the cluster dataset. This is intended to 
# the clustered dataset found in the Data folder. The command to call
# it is 
# cluster = read.csv("../Data/Combined_for_Clustering.csv")
# The user also specifies the name of the file that the dendrams will
# be written. The output is a pdf of the dendrograms for all of the 
# clustering methods allowed in hclust.

fastClust <- function(dat, distType = "euclidean", nms = paste(cluster$city, cluster$state, sep = ", "),
                     graphName = NULL) {
  
  clusters <- vector("list", 8)
  method <- c("ward.D", "ward.D2", "single", "complete", "average", 
             "mcquitty", "median", "centroid")
  
  d <- dist(dat, method = distType) # distance matrix
  
  for(i in 1:8) {
    clusters[[i]] <- hclust(d, method = method[i]) 
  }
  if(!is.null(graphName)) {
    pdf(graphName, height = 8.5, width = 11)
    for(i in 1:8) {
      plot(clusters[[i]], labels = nms, main = method[i], cex = 0.5, axes = FALSE, ylab = NA, xlab = NA)
    }
    dev.off()
  }
  pdf(graphName, height = 8.5, width = 11)
  for(i in 1:8) {
    plot(clusters[[i]], labels = nms, main = method[i], cex = 0.5, axes = FALSE, ylab = NA, xlab = NA)
  }
  dev.off()
  
  clusters
  
}

# The next set of functions are for the following function clustPlot.
# They create the clusters so that clustPlot can map them.
kmean <- function(dat) {
  k <- matrix(0, nrow = nrow(dat), ncol = 6)
  for(i in 1:6) {
    k[, i] <- pam(dat, i + 3)$clustering
  }
  colnames(k) <- c("C4", "C4", "C6", "C7", "C8", "C9")
  k
}

hcc <- function(dat, type) {
  tmp <- fastClust(dat, type)
  method <- c("ward.D", "ward.D2", "single", "complete", "average", 
             "mcquitty", "median", "centroid")
  clust <- vector("list", 8)
  for(i in 1:8) {
    clust[[i]] <- matrix(0, nrow = nrow(dat), ncol = 6)
    colnames(clust[[i]]) <- c("C4", "C5", "C6", "C7", "C8", "C9")
    for(j in 1:ncol(clust[[i]])) {
      clust[[i]][, j] <- cutree(tmp[[i]], j + 3)
    }
  }
  clust
}

clust.plot <- function(trimDat, clust, Plot.title, top = NULL) {
  tmp <- as.data.frame(cbind(trimDat, clust))
  tmp$state <- tolower(tmp$state)
  longitude <- c(-117.25, -108.50)
  latitude <- c(27.2, 27)
  
  tmp$latitude[tmp$state == "alaska"] <- latitude[1]
  tmp$latitude[tmp$state == "hawaii"] <- latitude[2]
  tmp$longitude[tmp$state == "alaska"] <- longitude[1]
  tmp$longitude[tmp$state == "hawaii"] <- longitude[2]
  
  clustGraphs <- vector("list", 6)
  
  for(i in 1:6) {
    
    cols <- brewer.pal(i + 3, "Set1")
    tmp$clust.col <- as.factor(tmp[, i + 3])

    clustGraphs[[i]] <- ggplot(tmp, aes(map_id = state)) +
      # map points to the fifty_states shape data
      geom_map(fill = "gray90", color = "black", map = fifty_states) +
      expand_limits(x = fifty_states$long, y = fifty_states$lat) +
      coord_map() +
      scale_x_continuous(breaks = NULL) +
      scale_y_continuous(breaks = NULL) +
      labs(x = "", y = "") +
      theme(legend.position = "bottom",
            panel.background = element_blank()) +
      geom_point(aes(x = longitude, y = latitude,
                                 colour = clust.col),
                             data = tmp, show.legend = FALSE) +
      scale_colour_manual(values = cols) +
      ggtitle(paste(Plot.title, i + 3, "clusters", sep = " "))
  }
  
  grid.arrange(clustGraphs[[1]], clustGraphs[[2]], clustGraphs[[3]], 
               clustGraphs[[4]], clustGraphs[[5]], clustGraphs[[6]], ncol = 2, top = top)
}


# The function clustPlot will create a series of graphs that show the 
# results of the clustering on a map for 6-9 clusters. The dat argument
# is the dataset that is used to compute the distances for clustering. 
# The trimDat argument is the state, lat, and lon information for 
# each weather station. The type argument is either "K-means" or 
# the distance measure used for the hclust function. The options 
# for type are: "K-means", "euclidean", "maximum", "manhattan", 
# "canberra", or "minkowski". The function outputs a ggplot that 
# has a map of the clusters on a US map. There are 4 maps on the 
# final graph. The hclust option will produce a bunch of graphs.
# There will be a graph for each clustering option so keep that in mind.

clustPlot <- function(dat = cc, trimDat = cluster[, c(33:35)], type = "K-means") {
  # Code to generate K-means plot
  if(type == "K-means") {
    tmp <- kmean(dat)
    clust.plot(trimDat, tmp, "K-Means")
  } else {
    tmp <- hcc(dat, type)
    method <- c("ward.D", "ward.D2", "single", "complete", "average", 
               "mcquitty", "median", "centroid")
    for(i in 1:8) {
      clust.plot(trimDat, tmp[[i]], type, method[i])
    }
  }
}
  


