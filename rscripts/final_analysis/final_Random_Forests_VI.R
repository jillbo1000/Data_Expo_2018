# Set working directory to source file location prior to running. 

library(dplyr)
library(lubridate)
library(tidyr)
library(randomForest)
library(mice)
library(VIM)
library(missForest)
library(ggplot2)
library(gridExtra)
library(RColorBrewer)

# Read in weather, forecast, and city data. 
rf.dat <- read.csv("../../data/summary_city_month_lag.csv")
summary(rf.dat)
colnames(rf.dat)

clusters <- read.csv("../../data/Cluster_Summary.csv")
head(clusters)

rf.dat <- left_join(rf.dat, clusters)
rf.dat <- select(rf.dat, -city, -state, -longitude, -latitude,
                 -mxT_sd, -mxT_mean, -mnT_sd, -mnT_mean,
                 -mxT_sd_abs, -mnT_sd_abs)
colnames(rf.dat)

#==============================================================================
#
#                                Imputation
#
#==============================================================================

# This lets you see the pattern of NAs. See the website 
# https://www.analyticsvidhya.com/blog/2016/03/tutorial-powerful-packages-imputing-missing-values/
# for an explanation of how to read it. The table is super 
# helpful.
colnames(rf.dat)
x <- rf.dat[, -1]
summary(x)
dim(x)
colnames(x)
md.pattern(x)

# This lets you visualize the missing data. See the website
# for information in interpreting the graph.
mice_plot <- aggr(x, col = c('navyblue','yellow'),
                  numbers = TRUE, sortVars=TRUE,
                  labels = names(rf.dat), cex.axis=.7,
                  gap = 3, ylab = c("Missing data","Pattern"))

# The rest of the code imputes the information from a nearby weather 
# station. 

# The first line checks to make sure that the data from the two subsets are in the same
# order so a direct substitution can be done. If the sum is 0 the values can be directly
# replaced without messing around with matching or merging.
sum(paste(rf.dat$month[rf.dat$AirPtCd == "KDMH"], rf.dat$lag[rf.dat$AirPtCd == "KDMH"]) != 
    paste(rf.dat$month[rf.dat$AirPtCd == "KDOV"], rf.dat$lag[rf.dat$AirPtCd == "KDOV"]))
rf.dat$Min_Vis[rf.dat$AirPtCd == "KDMH"] <- rf.dat$Min_Vis[rf.dat$AirPtCd == "KDOV"]
rf.dat$Sd_Vis[rf.dat$AirPtCd == "KDMH"] <- rf.dat$Sd_Vis[rf.dat$AirPtCd == "KDOV"]
rf.dat$CloudCover[rf.dat$AirPtCd == "KDMH"] <- rf.dat$CloudCover[rf.dat$AirPtCd == "KDOV"]
rf.dat$Sd_CloudCover[rf.dat$AirPtCd == "KDMH"] <- rf.dat$Sd_CloudCover[rf.dat$AirPtCd == "KDOV"]

sum(paste(rf.dat$month[rf.dat$AirPtCd == "KP68"], rf.dat$lag[rf.dat$AirPtCd == "KP68"]) != 
      paste(rf.dat$month[rf.dat$AirPtCd == "KRNO"], rf.dat$lag[rf.dat$AirPtCd == "KRNO"]))
rf.dat$Min_Vis[rf.dat$AirPtCd == "KP68"] <- rf.dat$Min_Vis[rf.dat$AirPtCd == "KRNO"]
rf.dat$Sd_Vis[rf.dat$AirPtCd == "KP68"] <- rf.dat$Sd_Vis[rf.dat$AirPtCd == "KRNO"]
rf.dat$CloudCover[rf.dat$AirPtCd == "KP68"] <- rf.dat$CloudCover[rf.dat$AirPtCd == "KRNO"]
rf.dat$Sd_CloudCover[rf.dat$AirPtCd == "KP68"] <- rf.dat$Sd_CloudCover[rf.dat$AirPtCd == "KRNO"]

sum(is.na(rf.dat)) # No NAs left in dataset after imputation


###############################################################################
#
#                           Variable Importance
#
###############################################################################

# Newer data did not need any imputed data
colnames(rf.dat)
predictors <- rf.dat[, c(2:29, 33:34)]
head(predictors)
colnames(predictors)

response <- rf.dat[, c(30:32)]
head(response)

# rf5.mx <- vector("list", 5)
# rf5.mn <- vector("list", 5)
# rf5.precip <- vector("list", 5)
# for(i in 1:5) {
#   tmp.pred <- predictors[rf.impute$Cluster5 == i, ]
#   tmp.res <- response[rf.impute$Cluster5 == i, ]
#   rf5.mx[[i]] <- randomForest(tmp.pred, tmp.res$mxT_mean_abs, keep.forest = TRUE)
#   rf5.mn[[i]] <- randomForest(tmp.pred, tmp.res$mnT_mean_abs, keep.forest = TRUE)
#   rf5.precip[[i]] <- randomForest(tmp.pred, tmp.res$BSS, keep.forest = TRUE)
# }

set.seed(1232913)
rf6.mx <- vector("list", 6)
rf6.mn <- vector("list", 6)
rf6.precip <- vector("list", 6)
for(i in 1:6) {
  tmp.pred <- predictors[rf.dat$Cluster6 == i, ]
  tmp.res <- response[rf.dat$Cluster6 == i, ]
  rf6.mx[[i]] <- randomForest(tmp.pred, tmp.res$mxT_mean_abs, scale = FALSE, 
                             keep.forest = TRUE, importance = TRUE)
  rf6.mn[[i]] <- randomForest(tmp.pred, tmp.res$mnT_mean_abs, scale = FALSE, 
                             keep.forest = TRUE, importance = TRUE)
  rf6.precip[[i]] <- randomForest(tmp.pred, tmp.res$BSS, scale = FALSE, 
                                 keep.forest = TRUE, importance = TRUE)
}

# rf7.mx <- vector("list", 7)
# rf7.mn <- vector("list", 7)
# rf7.precip <- vector("list", 7)
# for(i in 1:7) {
#   tmp.pred <- predictors[rf.impute$Cluster7 == i, ]
#   tmp.res <- response[rf.impute$Cluster7 == i, ]
#   rf7.mx[[i]] <- randomForest(tmp.pred, tmp.res$mxT_mean_abs, keep.forest = TRUE, 
#                              importance = TRUE)
#   rf7.mn[[i]] <- randomForest(tmp.pred, tmp.res$mnT_mean_abs, keep.forest = TRUE, 
#                              importance = TRUE)
#   rf7.precip[[i]] <- randomForest(tmp.pred, tmp.res$BSS, keep.forest = TRUE, 
#                                  importance = TRUE)
# }

# rf8.mx <- vector("list", 8)
# rf8.mn <- vector("list", 8)
# rf8.precip <- vector("list", 8)
# for(i in 1:8) {
#   tmp.pred <- predictors[rf.impute$Cluster8 == i, ]
#   tmp.res <- response[rf.impute$Cluster8 == i, ]
#   rf8.mx[[i]] <- randomForest(tmp.pred, tmp.res$mxT_mean_abs, keep.forest = TRUE)
#   rf8.mn[[i]] <- randomForest(tmp.pred, tmp.res$mnT_mean_abs, keep.forest = TRUE)
#   rf8.precip[[i]] <- randomForest(tmp.pred, tmp.res$BSS, keep.forest = TRUE)
# }

# pdf("../images/Random Forest VI Plots/Five Clusters First Run.pdf", width = 17, height = 11)
# par(mfrow = c(3, 3))
# for(i in 1:5) {
#   varImpPlot(rf5.mx[[i]], main = paste("Max Temp, Cluster ", i, sep = ""))
#   varImpPlot(rf5.mn[[i]], main = paste("Min Temp, Cluster ", i, sep = ""))
#   varImpPlot(rf5.precip[[i]], main = paste("BSS, Cluster ", i, sep = ""))
# }
# dev.off()

pdf("../../images/test/random_forest_six_clusters.pdf", width = 17, height = 11)
par(mfrow = c(3, 3))
for(i in 1:6) {
  varImpPlot(rf6.mx[[i]], main = paste("Max Temp, Cluster ", i, sep = ""))
  varImpPlot(rf6.mn[[i]], main = paste("Min Temp, Cluster ", i, sep = ""))
  varImpPlot(rf6.precip[[i]], main = paste("BSS, Cluster ", i, sep = ""))
}
dev.off()

# pdf("../images/Random Forest VI Plots/Seven Clusters First Run.pdf", width = 17, height = 11)
# par(mfrow = c(3, 3))
# for(i in 1:7) {
#   varImpPlot(rf7.mx[[i]], main = paste("Max Temp, Cluster ", i, sep = ""))
#   varImpPlot(rf7.mn[[i]], main = paste("Min Temp, Cluster ", i, sep = ""))
#   varImpPlot(rf7.precip[[i]], main = paste("BSS, Cluster ", i, sep = ""))
# }
# dev.off()
# 
# pdf("../images/Random Forest VI Plots/Eight Clusters First Run.pdf", width = 17, height = 11)
# par(mfrow = c(3, 3))
# for(i in 1:8) {
#   varImpPlot(rf8.mx[[i]], main = paste("Max Temp, Cluster ", i, sep = ""))
#   varImpPlot(rf8.mn[[i]], main = paste("Min Temp, Cluster ", i, sep = ""))
#   varImpPlot(rf8.precip[[i]], main = paste("BSS, Cluster ", i, sep = ""))
# }
# dev.off()


#===========================================================================
# Parallel Coordinate Plots
#===========================================================================

clustTitle <- c("Midwest", "Northeast", "Southeast", "Cali-Florida", 
               "Intermountain West", "Southwest")
cols <- brewer.pal(8, "Set1")[c(4, 2, 3, 8, 1, 5)]

# Create a subset of variables based on the top variable selection
# for each measure and each cluster

set <- function(dat.mx, dat.mn, dat.precip, num = 5, clustNum = 6) {
  
  vars <- vector("list", clustNum)
  
  for(i in 1:clustNum) {
    tmp1 <- dat.mx[[i]]$importance[, 1][order(dat.mx[[i]]$importance[, 1], decreasing = TRUE)]
    tmp1 <- as.data.frame((tmp1 - min(tmp1)) * 100 / max(tmp1 - min(tmp1)))
    tmp1 <- cbind(rownames(tmp1), tmp1)
    colnames(tmp1) <- c("Variable", "Importance")
    tmp1 <- tmp1[order(tmp1$Importance, decreasing = TRUE), ]
    
    tmp2 <- dat.mn[[i]]$importance[, 1][order(dat.mn[[i]]$importance[, 1], decreasing = TRUE)]
    tmp2 <- as.data.frame((tmp2 - min(tmp2)) * 100 / max(tmp2 - min(tmp2)))
    tmp2 <- cbind(rownames(tmp2), tmp2)
    colnames(tmp2) <- c("Variable", "Importance")
    tmp2 <- tmp2[order(tmp2$Importance, decreasing = TRUE), ]
    
    tmp3 <- dat.precip[[i]]$importance[, 1][order(dat.precip[[i]]$importance[, 1], decreasing = TRUE)]
    tmp3 <- as.data.frame((tmp3 - min(tmp3)) * 100 / max(tmp3 - min(tmp3)))
    tmp3 <- cbind(rownames(tmp3), tmp3)
    colnames(tmp3) <- c("Variable", "Importance")
    tmp3 <- tmp3[order(tmp3$Importance, decreasing = TRUE), ]
    
    vars[[i]] <- unique(c(as.character(tmp1$Variable[1:num]), 
                         as.character(tmp2$Variable[1:num]), 
                         as.character(tmp3$Variable[1:num])))
  }
  
  unique(c(vars[[1]], vars[[2]], vars[[3]], vars[[4]], vars[[5]], vars[[6]]))
}

set3 <- set(rf6.mx, rf6.mn, rf6.precip, num = 3)
set4 <- set(rf6.mx, rf6.mn, rf6.precip, num = 4)
set5 <- set(rf6.mx, rf6.mn, rf6.precip, num = 5)

# Clean up the labels
labels <- read.csv("../../data/varNameLabels.csv")
labels <- labels[order(labels$VarName), ]
labels$order <- 1:nrow(labels)
lab1 <- labels[, 1]
lab2 <- labels[, 2]
lab2 <- factor(lab2, levels = lab2[labels$order])

# Max Temp
mxt <- as.data.frame(names(rf6.mx[[1]]$importance[, 1]))
for(i in 1:6) {
  mxt <- cbind(mxt, rf6.mx[[i]]$importance[, 1])
}
mxt
colnames(mxt) <- c("Variable", clustTitle)
mxt$CleanVar <- lab2[match(as.character(mxt$Variable), as.character(labels$VarName))]
  
for(i in 1:6) {
  mxt[, i + 1] <- mxt[, i + 1] * 100 / max(mxt[, i + 1])
}

mxt <- mxt[order(as.character(mxt$CleanVar)), ]
mxt

mxt <- gather(mxt, "Region", "Importance", 2:7)
head(mxt)

mxt <- mxt[as.character(mxt$Variable) %in% set3, ]
mxt$Type <- "Maximum Temperature"

# Min Temp
mnt <- as.data.frame(names(rf6.mn[[1]]$importance[, 1]))
for(i in 1:6) {
  mnt <- cbind(mnt, rf6.mn[[i]]$importance[, 1])
}
mnt
colnames(mnt) <- c("Variable", clustTitle)
mnt$CleanVar <- lab2[match(as.character(mnt$Variable), as.character(labels$VarName))]

for(i in 1:6) {
  mnt[, i + 1] <- mnt[, i + 1] * 100 / max(mnt[, i + 1])
}

mnt <- mnt[order(as.character(mnt$CleanVar)), ]
mnt

mnt <- gather(mnt, "Region", "Importance", 2:7)
head(mnt)

mnt <- mnt[as.character(mnt$Variable) %in% set3, ]
mnt$Type <- "Minimum Temperature"

# Precipitation
precip <- as.data.frame(names(rf6.precip[[1]]$importance[, 1]))
for(i in 1:6) {
  precip <- cbind(precip, rf6.precip[[i]]$importance[, 1])
}
precip
colnames(precip) <- c("Variable", clustTitle)
precip$CleanVar <- lab2[match(as.character(precip$Variable), as.character(labels$VarName))]

for(i in 1:6) {
  precip[, i + 1] = precip[, i + 1] * 100 / max(precip[, i + 1])
}

precip <- precip[order(as.character(precip$Variable)), ]
precip

precip <- gather(precip, "Region", "Importance", 2:7)
head(precip)

precip <- precip[as.character(precip$Variable) %in% set3, ]
precip$Type <- "Precipitation"

combined <- rbind(mxt, mnt, precip)

# Parallel Coordinate Plots
gmax <- ggplot(combined, aes(x = CleanVar, y = Importance)) +
  geom_path(aes(group = Region, color = Region), size = 1) +
  facet_wrap(~Type, ncol = 1) +
  scale_color_manual(values = brewer.pal(8, "Set1")[c(8, 1, 4, 2, 3, 5)]) +
  xlab("") +
  ylab("Scaled Importance") +
  theme_bw() +
  # expand_limits(x = c(-0.3, 19.3)) +
  theme(#axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.x = element_text(angle = 20, vjust  = 1, hjust = 1, size = 12),
        legend.position = "none",
    strip.text = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 12)) 

# pdf("../images/Random Forest VI Plots/VI Parallel Coordinate Plot.pdf", height = 6.5, width = 9)
# gmax
# dev.off()

pdf("../../images/final/finalImportance_slides.pdf", height = 5, width = 8)
gmax
dev.off()

pdf("../../images/final/finalImportance.pdf", height = 5, width = 15)
gmax
dev.off()

pdf("../../images/test/VI Parallel Coordinate Plot2.pdf", height = 6.5, width = 25)
gmax
dev.off()


write.csv(combined, "../../data/Importance_Data.csv", row.names = FALSE)

# Add on a legend for the paper

gmax2 <- ggplot(combined, aes(x = CleanVar, y = Importance)) +
  geom_path(aes(group = Region, color = Region), size = 1) +
  facet_wrap(~Type, ncol = 1) +
  scale_color_manual("", values = brewer.pal(8, "Set1")[c(8, 1, 4, 2, 3, 5)]) +
  xlab("") +
  ylab("Scaled Importance") +
  theme_bw() +
  # expand_limits(x = c(-0.3, 19.3)) +
  theme(#axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.x = element_text(angle = 20, vjust  = 1, hjust = 1, size = 12),
    legend.position = "bottom",
    strip.text = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 12)) 

pdf("../../images/final/paper/finalImportance_Paper.pdf", height = 6, width = 10)
gmax2
dev.off()


#=============================================================================
# First, create a set of boxes that we can color differently. 
tbox.x <- c(0, 0, 6, 6, 0)
tbox.y <- c(0, 8, 8, 0, 0)

# Now create a series of 6 boxes. 
boxes <- data.frame(x = c(tbox.x, tbox.x+6, tbox.x+12, tbox.x+18, 
                          tbox.x+24, tbox.x+30),
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
  annotate("text", x = 3 + c(0, 6, 12, 18, 24, 30), 
           y = rep(4, 6), 
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

gmax2 <- ggplot(combined, aes(x = CleanVar, y = Importance)) +
  geom_path(aes(group = Region, color = Region), size = 1) +
  facet_wrap(~Type, ncol = 1) +
  scale_color_manual("", values = brewer.pal(8, "Set1")[c(8, 1, 4, 2, 3, 5)]) +
  xlab("") +
  ylab("Scaled Importance") +
  theme_bw() +
  # expand_limits(x = c(-0.3, 19.3)) +
  theme(#axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.x = element_text(angle = 20, vjust  = 1, hjust = 1, size = 12),
    legend.position = "none",
    strip.text = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 12)) 


pdf("../../images/final/paper/finalImportance.pdf", height = 6, width = 10)
grid.draw(rbind(ggplotGrob(gmax2), ggplotGrob(legend), size = "first"))
dev.off()

#===========================================================================
# Dot Charts
#===========================================================================

# Create a subset of variables based on the top variable selection
# for each measure and each cluster
set <- function(dat.mx, dat.mn, dat.precip, num = 5, clustNum = 6) {
  
  vars <- vector("list", clustNum)

  for(i in 1:clustNum) {
    tmp1 <- dat.mx[[i]]$importance[, 1][order(dat.mx[[i]]$importance[, 1], decreasing = TRUE)]
    tmp1 <- as.data.frame((tmp1 - min(tmp1)) * 100 / max(tmp1 - min(tmp1)))
    tmp1 <- cbind(rownames(tmp1), tmp1)
    colnames(tmp1) <- c("Variable", "Importance")
    tmp1 <- tmp1[order(tmp1$Importance, decreasing = TRUE), ]

    tmp2 <- dat.mn[[i]]$importance[, 1][order(dat.mn[[i]]$importance[, 1], decreasing = TRUE)]
    tmp2 <- as.data.frame((tmp2 - min(tmp2)) * 100 / max(tmp2 - min(tmp2)))
    tmp2 <- cbind(rownames(tmp2), tmp2)
    colnames(tmp2) <- c("Variable", "Importance")
    tmp2 <- tmp2[order(tmp2$Importance, decreasing = TRUE), ]

    tmp3 <- dat.precip[[i]]$importance[, 1][order(dat.precip[[i]]$importance[, 1], decreasing = TRUE)]
    tmp3 <- as.data.frame((tmp3 - min(tmp3)) * 100 / max(tmp3 - min(tmp3)))
    tmp3 <- cbind(rownames(tmp3), tmp3)
    colnames(tmp3) <- c("Variable", "Importance")
    tmp3 <- tmp3[order(tmp3$Importance, decreasing = TRUE), ]

    vars[[i]] <- unique(c(as.character(tmp1$Variable[1:num]), 
                         as.character(tmp2$Variable[1:num]), 
                         as.character(tmp3$Variable[1:num])))
  }

  unique(c(vars[[1]], vars[[2]], vars[[3]], vars[[4]], vars[[5]], vars[[6]]))
}

set3 <- set(rf6.mx, rf6.mn, rf6.precip, num = 3)
set4 <- set(rf6.mx, rf6.mn, rf6.precip, num = 4)
set5 <- set(rf6.mx, rf6.mn, rf6.precip, num = 5)

clustTitle <- c("Midwest", "Northeast", "South", "Pacific Coast", 
               "Intermountain West", "Southwest")

dot <- function(dat.mx, dat.mn, dat.precip, sset = set5, clustNum = 6, 
               setnum = 5, cTitle = clustTitle) {
  
  dots <- vector("list", clustNum)
  
  for(i in 1:clustNum) {
    tmp1 <- dat.mx[[i]]$importance[, 1][order(dat.mx[[i]]$importance[, 1], decreasing = TRUE)]
    tmp1 <- as.data.frame((tmp1 - min(tmp1)) * 100 / max(tmp1 - min(tmp1)))
    tmp1 <- cbind(rownames(tmp1), tmp1)
    colnames(tmp1) <- c("Variable", "Importance")
    tmp1$Measure <- "Maximum Temperature"
    # tmp1$Variable <- factor(as.character(tmp1$Variable), 
    #                        levels = tmp1$Variable[order(tmp1$Importance, decreasing = FALSE)], 
    #                        ordered=TRUE)
    tmp1 <- tmp1[order(tmp1$Importance, decreasing = TRUE), ]
    tmp1$Size <- 1
    tmp1$Size[1:setnum] <- 1.5
    
    tmp2 <- dat.mn[[i]]$importance[, 1][order(dat.mn[[i]]$importance[, 1], decreasing = TRUE)]
    tmp2 <- as.data.frame((tmp2 - min(tmp2)) * 100 / max(tmp2 - min(tmp2)))
    tmp2 <- cbind(rownames(tmp2), tmp2)
    colnames(tmp2) <- c("Variable", "Importance")
    tmp2$Measure <- "Minimum Temperature"
    tmp2 <- tmp2[order(tmp2$Importance, decreasing = TRUE), ]
    tmp2$Size <- 1
    tmp2$Size[1:setnum] <- 1.5
    
    tmp3 <- dat.precip[[i]]$importance[, 1][order(dat.precip[[i]]$importance[, 1], decreasing = TRUE)]
    tmp3 <- as.data.frame((tmp3 - min(tmp3)) * 100 / max(tmp3 - min(tmp3)))
    tmp3 <- cbind(rownames(tmp3), tmp3)
    colnames(tmp3) <- c("Variable", "Importance")
    tmp3$Measure <- "Precipitation"
    tmp3 <- tmp3[order(tmp3$Importance, decreasing = TRUE), ]
    tmp3$Size <- 1
    tmp3$Size[1:setnum] <- 1.5
    
    tmp <- rbind(tmp1, tmp2, tmp3)
    tmp$Measure <- factor(tmp$Measure, levels = c("Maximum Temperature", 
                                                 "Minimum Temperature", 
                                                 "Precipitation"))

    tmp <- tmp[as.character(tmp$Variable) %in% sset, ]
    tmp$Variable <- factor(tmp$Variable, levels = sort(unique(as.character(tmp$Variable))))
    tmp <- tmp[order(as.character(tmp$Variable)), ]
    
    col <- unique(c(as.character(tmp1$Variable[1:setnum]), 
                 as.character(tmp2$Variable[1:setnum]), 
                 as.character(tmp3$Variable[1:setnum])))
    a <- ifelse(levels(tmp$Variable) %in% col, "black", "gray")
    
    g1 <- ggplot(data = tmp, aes(x = Importance, y = Variable)) +
      geom_point(aes(color = Measure, size = Size)) +
      geom_path(aes(group = Measure, color = Measure)) +
      scale_color_manual(values = c("firebrick3", "dodgerblue3", "lightskyblue4"), guide = FALSE) +
      scale_size(range = c(1, 2), guide = FALSE) +
      ggtitle(cTitle[i]) +
      xlim(0, 100) +
      ylab("") + 
      facet_wrap(~Measure, nrow = 1) +
      theme_bw() +
      theme(axis.text.y = element_text(colour = a))
    
    dots[[i]] = g1
  }
  
  dots
}

dots5 <- dot(rf6.mx, rf6.mn, rf6.precip, sset = set5, setnum = 5)

pdf("../../images/test/Dot Plots Max 5.3.pdf", width = 15, height = 10)
grid.arrange(dots5[[4]],
             dots5[[2]],
             dots5[[5]],
             dots5[[1]],
             dots5[[6]],
             dots5[[3]], ncol = 2)
dev.off()

dots4 <- dot(rf6.mx, rf6.mn, rf6.precip, sset = set4, setnum = 4)

pdf("../../images/test/Dot Plots Max 4.3.pdf", width = 15, height = 10)
grid.arrange(dots4[[4]],
             dots4[[2]],
             dots4[[5]],
             dots4[[1]],
             dots4[[6]],
             dots4[[3]], ncol = 2)
dev.off()

dots3 <- dot(rf6.mx, rf6.mn, rf6.precip, sset = set3, setnum = 3)

pdf("../../images/test/Dot Plots Max 3.3.pdf", width = 15, height = 10)
grid.arrange(dots3[[4]],
             dots3[[2]],
             dots3[[5]],
             dots3[[1]],
             dots3[[6]],
             dots3[[3]], ncol = 2)
dev.off()
