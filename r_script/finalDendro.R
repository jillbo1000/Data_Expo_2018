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
library(ggdendro)

cluster2 = read.csv("../Data/summary_city.csv")
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
                                  -BSS, -Cluster6, -color, -ClustName)

cc2 = scale(cc2)

d2 <- dist(cc2, method = "euclidean")
c2 <- hclust(d2, method = "ward.D2")
l2 <- cutree(c2, k = 6)

# cluster2$Cluster <- l2

# Define color scheme
set1 <- c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#f781bf", "gray50")
set1 <- set1[c(4, 2, 3, 6, 1, 5, 7)] # Reorder to match the original layout


# Code adapted from answer given  by "jlhoward" at:
# https://stackoverflow.com/questions/21474388/colorize-clusters-in-dendogram-with-ggplot2
#=============================================================================
dendr <- dendro_data(c2, type="rectangle") # convert for ggplot
dendr[["labels"]]$label <- cluster2$name[as.numeric(as.character(dendr[["labels"]]$label))]
clust.df <- data.frame(label = cluster2$name, cluster=factor(l2))
# dendr[["labels"]] has the labels, merge with clust.df based on label column
dendr[["labels"]] <- merge(dendr[["labels"]],clust.df, by="label")
# plot the dendrogram; note use of color=cluster in geom_text(...)
p1 <- ggplot() + 
  geom_segment(data=segment(dendr), aes(x=x, y=y, xend=xend, yend=yend)) + 
  geom_text(data=label(dendr), aes(x, y, label=label, hjust=0, color=cluster), 
            size=6) +
  coord_flip() + scale_y_reverse(expand=c(0, 0), limits = c(40, -15)) + 
  scale_color_manual(values = set1) + 
  theme(legend.position = "none",
    axis.line=element_blank(),
        axis.ticks=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background=element_rect(fill="white"),
        panel.grid=element_blank())
#=============================================================================

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
#=============================================================================

pdf(height = 25, width = 10, file = "../images/final/paper/finalDendro.pdf")
#grid.arrange(grobs = list(ggplotGrob(p1), ggplotGrob(legend)), heights = c(0.9, 0.1))
p1
dev.off()