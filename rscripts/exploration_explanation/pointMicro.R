# Test script for point-based micromapss... (I hope they work)

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
library(GGally)

cluster2 = read.csv("../../data/summary_city.csv")
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

# Now, subset the results of the cluster.
cc2 <- cluster2 %>% dplyr::select(-AirPtCd, -city, -state, -longitude, -latitude,
                                  -mxT_sd, -mxT_mean, -mnT_sd, -mnT_mean,
                                  -mxT_sd_abs, -mxT_mean_abs, -mnT_sd_abs, -mnT_mean_abs,
                                  -BSS)

cc2 <- scale(cc2)

PMplot <- function(cluster2, method1 = "euclidean", method2 = "ward.D2", clustNum = 5){
  cc2 <- cluster2 %>% dplyr::select(-AirPtCd, -city, -state, -longitude, -latitude,
                                    -mxT_sd, -mxT_mean, -mnT_sd, -mnT_mean,
                                    -mxT_sd_abs, -mxT_mean_abs, -mnT_sd_abs, -mnT_mean_abs,
                                    -BSS)
  
  cc2 <- scale(cc2)
  
  d2 <- dist(cc2, method = method1)
  c2 <- hclust(d2, method = method2)
  l2 <- cutree(c2, k = clustNum)
  
  cluster2$Cluster <- l2
  len <- clustNum
  
  clusterSum <- cluster2 %>% group_by(Cluster) %>% summarize(mlat = mean(mxT_mean_abs))
  
  cluster2 <- left_join(cluster2, clusterSum, by = "Cluster") %>% arrange(mlat, mxT_mean_abs)
  print(unique(cluster2$AirPtCd))
  # Now arrange and redo factor levels. 
  tmp <- as.data.frame(cluster2)
  tmp$state <- as.character(tmp$state)
  # Help for abbreviations from:
  # - https://stackoverflow.com/questions/5411979/state-name-to-abbreviation-in-r
  tmp$stateABB <- "none"
  for(i in 1:nrow(tmp)){tmp$stateABB[i] <- state.abb[grep(tmp$state[i], state.name)]}
  tmp$state <- tolower(tmp$state)
  longitude <- c(-117.25, -108.50)
  latitude <- c(27.2, 27)
  
  # Create copy of tmp file where we adjust the coordinates for alaska and hawaii
  tmp2 <- tmp
  tmp2$latitude[tmp2$state == "alaska"] <- latitude[1]
  tmp2$latitude[tmp2$state == "hawaii"] <- latitude[2]
  tmp2$longitude[tmp2$state == "alaska"] <- longitude[1]
  tmp2$longitude[tmp2$state == "hawaii"] <- longitude[2]
  
  tmp$name <- paste(tmp$city, tmp$stateABB, sep = ", ")
  
  # pdf("tester.pdf", height = 20)
  # ggplot(cluster2, aes(y = AirPtCd, x = Max_Temp)) + geom_point()
  # dev.off()
  col <- brewer.pal(len, "Set1")
  
  # tmp <- arrange(tmp, mlat, latitude)
  tmp$AirPtCd <- factor(tmp$AirPtCd, levels = tmp$AirPtCd)
  tmp$Cluster <- factor(tmp$Cluster, levels = unique(tmp$Cluster))
  tmp2$Cluster <- factor(tmp2$Cluster, levels = unique(tmp$Cluster))
  
  maps <- temps <- vector("list", len)
  tmpcor <- tmp %>% group_by(Cluster) %>% 
    summarize(c1 = cor(mxT_mean_abs, mnT_mean_abs, method = "kendall"),
              c2 = cor(mnT_mean_abs, -BSS, method = "kendall"),
              c3 = cor(mxT_mean_abs, -BSS, method = "kendall"))
  
  for(i in 1:len){
    maps[[i]] <- ggplot(tmp2, aes(map_id = state)) +
      # map points to the fifty_states shape data
      geom_map(fill = "gray90", color = "gray80", map = fifty_states) +
      expand_limits(x = fifty_states$long, y = fifty_states$lat) +
      coord_map() +
      scale_x_continuous(breaks = NULL) +
      scale_y_continuous(breaks = NULL) +
      labs(x = "", y = "") +
      theme(legend.position = "bottom",
            panel.background = element_blank()) +
      geom_point(aes(x = longitude, y = latitude),
                 col = col[len+1-i], size = 1,
                 data = tmp2[as.numeric(tmp2$Cluster) == len+1-i, ], show.legend = FALSE) + 
      geom_point(aes(x = longitude, y = latitude),
                 col = "gray50", size = 0.5,
                 data = tmp2[as.numeric(tmp2$Cluster) != len+1-i, ], show.legend = FALSE) + 
      annotate("text", x = -100, y = 22, label = paste("MaxTemp/MinTemp:", round(tmpcor[len+1-i, 2], 3)),
               size = 2) + 
      annotate("text", x = -100, y = 18, label = paste("MinTemp/Precip:", round(tmpcor[len+1-i, 3], 3)),
               size = 2) + 
      annotate("text", x = -100, y = 14, label = paste("MaxTemp/Precip:", round(tmpcor[len+1-i, 4], 3)),
               size = 2) 
    
    
    # temps[[i]] <- ggplot(tmp[tmp$Cluster == i, ], aes(y = factor(name), x = Max_Temp)) +
    #   geom_point(col = col[i]) +
    #   xlim(45, 91) + 
    #   theme(y.axis.)
  }
  
  
  #tmp <- arrange(tmp, desc(Cluster))
  tmp$name <- factor(tmp$name, levels = tmp$name)
  # latsALL <- ggplot(tmp, aes(x = factor(name), y = latitude)) +
  #   geom_point(aes(col = Cluster)) +
  #   geom_line(aes(group = Cluster, col = Cluster)) + 
  #   scale_color_manual(values = col) +
  #   theme(legend.position = "none",
  #         axis.title.y = element_blank()) + 
  #   coord_flip()
  
  
  tempsALL <- ggplot(tmp, aes(x = factor(name), y = mxT_mean_abs)) +
    geom_point(aes(col = Cluster)) +
    geom_line(aes(group = Cluster, col = Cluster)) + 
    scale_color_manual(values = col) +
    theme(legend.position = "none",
          axis.title.y = element_blank()) + 
    coord_flip()
  
  tempsALL_min <- ggplot(tmp, aes(x = factor(name), y = mnT_mean_abs)) +
    geom_point(aes(col = factor(Cluster))) +
    geom_line(aes(group = Cluster, col = Cluster)) + 
    scale_color_manual(values = col) +
    theme(legend.position = "none",
          axis.title.y = element_blank(),
          axis.text.y = element_blank()) + 
    coord_flip()
  
  precipALL <- ggplot(tmp, aes(x = factor(name), y = -BSS)) +
    geom_point(aes(col = factor(Cluster))) +
    geom_line(aes(group = Cluster, col = Cluster)) + 
    scale_color_manual(values = col) +
    theme(legend.position = "none",
          axis.title.y = element_blank(),
          axis.text.y = element_blank()) + 
    coord_flip()
  
  return(list(maps, tempsALL, tempsALL_min, precipALL))
}

# Now plot the results
p1 <- PMplot(cluster2, method1 = "euclidean", method2 = "ward.D2", clustNum = 5)
p2 <- PMplot(cluster2, method1 = "manhattan", method2 = "ward.D2", clustNum = 5)
p3 <- PMplot(cluster2, method1 = "euclidean", method2 = "ward.D2", clustNum = 6)
p4 <- PMplot(cluster2, method1 = "manhattan", method2 = "ward.D2", clustNum = 6)

pdf("../../images/micromaps/pointMicroTest.pdf", height = 15, width = 10)
grid.arrange(p1[[1]][[1]], p1[[1]][[2]], p1[[1]][[3]], p1[[1]][[4]], p1[[1]][[5]], 
             p1[[2]], p1[[3]], p1[[4]],
             layout_matrix = matrix(c(1, 2, 3, 4, 5, 6, 6, 6, 6, 6,
                                      7, 7, 7, 7, 7, 8, 8, 8, 8, 8),
                                    ncol = 4, byrow = FALSE),
             widths = c(.2, .3, .25, .25),
             top= "All Variables: euclidean distance and ward.D2 method."
)
dev.off()

pdf("../../images/micromaps/pointMicroTest2.pdf", height = 15, width = 10)
grid.arrange(p2[[1]][[1]], p2[[1]][[2]], p2[[1]][[3]], p2[[1]][[4]], p2[[1]][[5]], 
             p2[[2]], p2[[3]], p2[[4]],
             layout_matrix = matrix(c(1, 2, 3, 4, 5, 6, 6, 6, 6, 6,
                                      7, 7, 7, 7, 7, 8, 8, 8, 8, 8),
                                    ncol = 4, byrow = FALSE),
             widths = c(.2, .3, .25, .25),
             top= "All Variables: manhattan distance and ward.D2 method."
)
dev.off()

pdf("../../images/micromaps/pointMicroTest3.pdf", height = 15, width = 10)
grid.arrange(p3[[1]][[1]], p3[[1]][[2]], p3[[1]][[3]], p3[[1]][[4]], p3[[1]][[5]], p3[[1]][[6]], 
             p3[[2]], p3[[3]], p3[[4]],
             layout_matrix = matrix(c(1, 2, 3, 4, 5, 6, 
                                      7, 7, 7, 7, 7, 7,
                                      8, 8, 8, 8, 8, 8,
                                      9, 9, 9, 9, 9, 9
             ),
             ncol = 4, byrow = FALSE),
             widths = c(.2, .3, .25, .25),
             top= "All Variables: euclidean distance and ward.D2 method."
)
dev.off()

pdf("../../images/micromaps/pointMicroTest4.pdf", height = 15, width = 10)
grid.arrange(p4[[1]][[1]], p4[[1]][[2]], p4[[1]][[3]], p4[[1]][[4]], p4[[1]][[5]], p4[[1]][[6]], 
             p4[[2]], p4[[3]], p4[[4]],
             layout_matrix = matrix(c(1, 2, 3, 4, 5, 6, 
                                      7, 7, 7, 7, 7, 7,
                                      8, 8, 8, 8, 8, 8,
                                      9, 9, 9, 9, 9, 9
             ),
             ncol = 4, byrow = FALSE),
             widths = c(.2, .3, .25, .25),
             top= "All Variables: manhattan distance and ward.D2 method."
)
dev.off()

dev.off()


# Try a simple scatterplot matrix of the error variables. 
#==============================================================================
cluster2 <- read.csv("../../data/Combined_for_Clustering_Bean.csv")
colnames(cluster2)
summary(cluster2)

# Check for missing values
apply(cluster2, 2, function(x) sum(is.na(x)))

cluster2[cluster2$AirPtCd == "KDMH", ]
cluster2[cluster2$AirPtCd == "KP68", ]
cluster2$Min_Vis[cluster2$AirPtCd == "KDMH"] <- cluster2$Min_Vis[cluster2$AirPtCd == "KDOV"]
#cluster2$Max_Vis[cluster2$AirPtCd == "KDMH"] <- cluster2$Max_Vis[cluster2$AirPtCd == "KDOV"]
#cluster2$Mean_Vis[cluster2$AirPtCd == "KDMH"] <- cluster2$Mean_Vis[cluster2$AirPtCd == "KDOV"]
cluster2$Sd_Vis[cluster2$AirPtCd == "KDMH"] <- cluster2$Sd_Vis[cluster2$AirPtCd == "KDOV"]
#cluster2$Range_Vis[cluster2$AirPtCd == "KDMH"] <- cluster2$Range_Vis[cluster2$AirPtCd == "KDOV"]
cluster2$CloudCover[cluster2$AirPtCd == "KDMH"] <- cluster2$CloudCover[cluster2$AirPtCd == "KDOV"]
cluster2$Sd_CloudCover[cluster2$AirPtCd == "KDMH"] <- cluster2$Sd_CloudCover[cluster2$AirPtCd == "KDOV"]
cluster2$Sd_Mean_Wind_Speed[cluster2$AirPtCd == "KDMH"] <- cluster2$Sd_Mean_Wind_Speed[cluster2$AirPtCd == "KDOV"]
cluster2$Sd_Max_Wind_Speed[cluster2$AirPtCd == "KDMH"] <- cluster2$Sd_Max_Wind_Speed[cluster2$AirPtCd == "KDOV"]
cluster2$Max_Wind_Speed[cluster2$AirPtCd == "KDMH"] <- cluster2$Max_Wind_Speed[cluster2$AirPtCd == "KDOV"]
cluster2$Mean_Wind_Speed[cluster2$AirPtCd == "KDMH"] <- cluster2$Mean_Wind_Speed[cluster2$AirPtCd == "KDOV"]
cluster2$Max_Gust[cluster2$AirPtCd == "KDMH"] <- cluster2$Max_Gust[cluster2$AirPtCd == "KDOV"]
cluster2$Sd_Max_Gust[cluster2$AirPtCd == "KDMH"] <- cluster2$Sd_Max_Gust[cluster2$AirPtCd == "KDOV"]

cluster2$CloudCover[cluster2$AirPtCd == "KP68"] <- cluster2$CloudCover[cluster2$AirPtCd == "KRNO"]
cluster2$Sd_CloudCover[cluster2$AirPtCd == "KP68"] <- cluster2$Sd_CloudCover[cluster2$AirPtCd == "KRNO"]
cluster2$Min_Vis[cluster2$AirPtCd == "KP68"] <- cluster2$Min_Vis[cluster2$AirPtCd == "KRNO"]
cluster2$Sd_Vis[cluster2$AirPtCd == "KP68"] <- cluster2$Sd_Vis[cluster2$AirPtCd == "KRNO"]

apply(cluster2, 2, function(x) sum(is.na(x))) # All missing values taken care of. 


cc2 <- cluster2 %>% dplyr::select(-AirPtCd, -longitude, -latitude, -Max_Gust, -Sd_Max_Gust)
cc2 <- scale(cc2)
d2 <- dist(cc2, method = "euclidean")
c2 <- hclust(d2, method = "ward.D2")
l2 <- cutree(c2, k = 6)

cluster2$Cluster <- l2

city <- read.csv("../../data/locationsFinal.csv") %>%
  dplyr::select(AirPtCd, city, state)
head(city)
colnames(city)
summary(city)
city$city <- as.character(city$city)

cluster2 <- left_join(cluster2, city, by = "AirPtCd")

# Add the forecast accuracy information
cluster <- read.csv("../../data/Combined_for_Clustering.csv") %>%
  dplyr::select(AirPtCd, mxT_mean_abs, mxT_sd_abs, mnT_mean_abs, mnT_sd_abs)
cluster2 <- left_join(cluster2, cluster, by = "AirPtCd")

# Add precip forecast accuracy info
precips <- read.csv("../../data/PrecipErrors.csv")
cluster2 <- left_join(cluster2, precips, by = "AirPtCd")

tmp <- cluster2 %>% dplyr::select(Cluster, mxT_mean_abs, mnT_mean_abs, BSS)

colP <- RColorBrewer::brewer.pal(5, "Set1")

tmp$Cluster <- factor(tmp$Cluster, levels = 1:length(unique(tmp$Cluster)))
pm <- ggpairs(tmp, mapping = ggplot2::aes(color = Cluster), 
              lower = list(continuous = wrap("points", alpha = 0.3)))

for(i in 1:pm$nrow) {
  for(j in 1:pm$ncol){
    pm[i,j] <- pm[i,j] + 
      scale_color_brewer(type = "qual", palette = "Set1") + 
      scale_fill_brewer(type = "qual", palette = "Set1")
  }
}


cluster2$state <- tolower(cluster2$state)
longitude <- c(-117.25, -108.50)
latitude <- c(27.2, 27)

# Create copy of tmp file where we adjust the coordinates for alaska and hawaii
cluster2$latitude[cluster2$state == "alaska"] <- latitude[1]
cluster2$latitude[cluster2$state == "hawaii"] <- latitude[2]
cluster2$longitude[cluster2$state == "alaska"] <- longitude[1]
cluster2$longitude[cluster2$state == "hawaii"] <- longitude[2]
m1 <- ggplot(cluster2, aes(map_id = state)) +
  # map points to the fifty_states shape data
  geom_map(fill = "gray90", color = "gray80", map = fifty_states) +
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme(legend.position = "bottom",
        panel.background = element_blank()) +
  geom_point(aes(x = longitude, y = latitude, col = factor(Cluster)),
             size = 2) + 
  scale_color_brewer(type = "qual", palette = "Set1") + 
  theme(legend.position = "none")


pdf("../../images/cluster/clusterMat.pdf")
m1
pm
dev.off()


# Help on PCP plots from 
# https://www.r-bloggers.com/parallel-coordinate-plots-for-discrete-and-categorical-data-in-r-a-comparison/
library(GGally)
library(reshape2)
cc2 <- as.data.frame(cc2)
cc2$cluster <- as.factor(cluster2$Cluster)
cc2$id <- cluster2$AirPtCd
tst1 <- ggparcoord(cc2, columns = 1:28, groupColumn = "cluster", order = "anyClass") +
  scale_color_brewer(type = "qual", palette = "Set1") +
  ylim(-5.5, 5.5)

tst1$data$variable

cc2.sd <- cc2 %>% dplyr::select(contains("Sd"), elevation, dist2coast, cluster, id)
cc2.other <- cc2 %>% dplyr::select(-contains("Sd"))

cc2.sd.long <- melt(cc2.sd, id.vars = c("cluster", "id"))
cc2.sd.long$value[is.element(cc2.sd.long$variable, c("elevation", "dist2coast"))] <- 0
cc2.other.long <- melt(cc2.other, id.vars = c("cluster", "id"))

p1 <- ggplot(cc2.sd.long, aes(x = variable, y = value, group = factor(id))) + 
  geom_path(aes(color = factor(cluster)), alpha = 0.3) +
  scale_color_brewer(type = "qual", palette = "Set1") + 
  geom_path(data = cc2.sd.long[is.element(cc2.sd.long$id, c("KAAO", "KACY")), ],
            aes(x = variable, y = value, group = factor(id), color = factor(cluster)), 
            lwd = 1.5) + 
  theme(legend.position = "none")

p2 <- ggplot(cc2.other.long, aes(x = variable, y = value, group = factor(id))) + 
  geom_path(aes(color = factor(cluster)), alpha = 0.3) +
  scale_color_brewer(type = "qual", palette = "Set1") + 
  geom_path(data = cc2.other.long[is.element(cc2.sd.long$id, c("KAAO", "KACY")), ],
            aes(x = variable, y = value, group = factor(id), color = factor(cluster)), 
            lwd = 1.5) + 
  theme(legend.position = "none")


pdf("../../images/micromaps/pcptest.pdf")
grid.arrange(p1, p2, ncol = 1)
dev.off()








