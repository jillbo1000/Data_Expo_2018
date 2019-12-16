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
library(gridExtra)
library(grid)

# Read in weather, forecast, and city data. 
rf.dat <- read.csv("../Data/summary_city_month_lag.csv")

clusters <- read.csv("../Data/Cluster_Summary.csv")

rf.dat <- left_join(rf.dat, clusters)
rf.dat <- select(rf.dat, -city, -state, -longitude, -latitude,
                 -mxT_sd, -mxT_mean, -mnT_sd, -mnT_mean,
                 -mxT_sd_abs, -mnT_sd_abs)

#==============================================================================
#
#                                Imputation
#
#==============================================================================
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

###############################################################################
#
#                           Variable Importance
#
###############################################################################

# Newer data did not need any imputed data
predictors = rf.dat[, c(2:29, 33:34)]

response = rf.dat[, c(30:32)]

rf6.mx = vector("list", 6)
rf6.mn = vector("list", 6)
rf6.precip = vector("list", 6)
for(i in 1:6) {
  tmp.pred = predictors[rf.dat$Cluster6 == i, ]
  tmp.res = response[rf.dat$Cluster6 == i, ]
  rf6.mx[[i]] = randomForest(tmp.pred, tmp.res$mxT_mean_abs, 
                             keep.forest = TRUE, importance = TRUE)
  rf6.mn[[i]] = randomForest(tmp.pred, tmp.res$mnT_mean_abs, 
                             keep.forest = TRUE, importance = TRUE)
  rf6.precip[[i]] = randomForest(tmp.pred, tmp.res$BSS, 
                                 keep.forest = TRUE, importance = TRUE)
}


#===========================================================================
# Dot Charts
#===========================================================================

# Create a subset of variables based on the top variable selection
# for each measure and each cluster
set = function(dat.mx, dat.mn, dat.precip, num = 5, clustNum = 6) {
  
  vars = vector("list", clustNum)

  for(i in 1:clustNum) {
    tmp1 = dat.mx[[i]]$importance[, 1][order(dat.mx[[i]]$importance[, 1], decreasing = TRUE)]
    tmp1 = as.data.frame((tmp1 - min(tmp1)) * 100 / max(tmp1 - min(tmp1)))
    tmp1 = cbind(rownames(tmp1), tmp1)
    colnames(tmp1) = c("Variable", "Importance")
    tmp1 = tmp1[order(tmp1$Importance, decreasing = TRUE), ]

    tmp2 = dat.mn[[i]]$importance[, 1][order(dat.mn[[i]]$importance[, 1], decreasing = TRUE)]
    tmp2 = as.data.frame((tmp2 - min(tmp2)) * 100 / max(tmp2 - min(tmp2)))
    tmp2 = cbind(rownames(tmp2), tmp2)
    colnames(tmp2) = c("Variable", "Importance")
    tmp2 = tmp2[order(tmp2$Importance, decreasing = TRUE), ]

    tmp3 = dat.precip[[i]]$importance[, 1][order(dat.precip[[i]]$importance[, 1], decreasing = TRUE)]
    tmp3 = as.data.frame((tmp3 - min(tmp3)) * 100 / max(tmp3 - min(tmp3)))
    tmp3 = cbind(rownames(tmp3), tmp3)
    colnames(tmp3) = c("Variable", "Importance")
    tmp3 = tmp3[order(tmp3$Importance, decreasing = TRUE), ]

    vars[[i]] = unique(c(as.character(tmp1$Variable[1:num]), 
                         as.character(tmp2$Variable[1:num]), 
                         as.character(tmp3$Variable[1:num])))
  }

  unique(c(vars[[1]], vars[[2]], vars[[3]], vars[[4]], vars[[5]], vars[[6]]))
}

set3 = set(rf6.mx, rf6.mn, rf6.precip, num = 3)

clustTitle = c("Midwest", "Northeast", "South", "Pacific Coast", 
               "Intermountain West", "Southwest")

dot = function(dat.mx, dat.mn, dat.precip, sset = set5, clustNum = 6, 
               setnum = 5, cTitle = clustTitle) {
  
  pal2 <- brewer.pal(8, "Set1")[c(1:5, 8)]
  pal2 <- pal2[c(4, 2, 3, 6, 1, 5)]
  
  dots = vector("list", clustNum)
  
  for(i in 1:clustNum) {
    tmp1 = dat.mx[[i]]$importance[, 1][order(dat.mx[[i]]$importance[, 1], decreasing = TRUE)]
    tmp1 = as.data.frame((tmp1 - min(tmp1)) * 100 / max(tmp1 - min(tmp1)))
    tmp1 = cbind(rownames(tmp1), tmp1)
    colnames(tmp1) = c("Variable", "Importance")
    tmp1$Measure = "Maximum Temperature"
    # tmp1$Variable = factor(as.character(tmp1$Variable), 
    #                        levels = tmp1$Variable[order(tmp1$Importance, decreasing = FALSE)], 
    #                        ordered=TRUE)
    tmp1 = tmp1[order(tmp1$Importance, decreasing = TRUE), ]
    tmp1$Size = 1
    tmp1$Size[1:setnum] = 1.5
    
    tmp2 = dat.mn[[i]]$importance[, 1][order(dat.mn[[i]]$importance[, 1], decreasing = TRUE)]
    tmp2 = as.data.frame((tmp2 - min(tmp2)) * 100 / max(tmp2 - min(tmp2)))
    tmp2 = cbind(rownames(tmp2), tmp2)
    colnames(tmp2) = c("Variable", "Importance")
    tmp2$Measure = "Minimum Temperature"
    tmp2 = tmp2[order(tmp2$Importance, decreasing = TRUE), ]
    tmp2$Size = 1
    tmp2$Size[1:setnum] = 1.5
    
    tmp3 = dat.precip[[i]]$importance[, 1][order(dat.precip[[i]]$importance[, 1], decreasing = TRUE)]
    tmp3 = as.data.frame((tmp3 - min(tmp3)) * 100 / max(tmp3 - min(tmp3)))
    tmp3 = cbind(rownames(tmp3), tmp3)
    colnames(tmp3) = c("Variable", "Importance")
    tmp3$Measure = "Precipitation"
    tmp3 = tmp3[order(tmp3$Importance, decreasing = TRUE), ]
    tmp3$Size = 1
    tmp3$Size[1:setnum] = 1.5
    
    tmp = rbind(tmp1, tmp2, tmp3)
    tmp$Measure = factor(tmp$Measure, levels = c("Maximum Temperature", 
                                                 "Minimum Temperature", 
                                                 "Precipitation"))

    tmp = tmp[as.character(tmp$Variable) %in% sset, ]
    tmp$Variable = factor(tmp$Variable, levels = sort(unique(as.character(tmp$Variable))))
    tmp = tmp[order(as.character(tmp$Variable)), ]
    
    col = unique(c(as.character(tmp1$Variable[1:setnum]), 
                 as.character(tmp2$Variable[1:setnum]), 
                 as.character(tmp3$Variable[1:setnum])))
    a = ifelse(levels(tmp$Variable) %in% col, "black", "gray")
    
    g1 = ggplot(data = tmp, aes(x = Importance, y = Variable)) +
      geom_point(aes(size = Size)) +
      geom_path(aes(group = Measure)) +
      #scale_color_manual(values = c("firebrick3", "dodgerblue3", "lightskyblue4"), guide = FALSE) +
      scale_size(range = c(1, 2), guide = FALSE) +
      #ggtitle(cTitle[i]) +
      xlim(0, 100) +
      ylab("") + 
      facet_wrap(~Measure, nrow = 1) +
      theme_bw()+
      theme(axis.text.y = element_text(colour = a),
            strip.background = element_rect(fill = pal2[i]),
            strip.text = element_text(size = 14))
    
    if(i != 1){g1 <- g1 + theme(axis.title.x = element_blank())}
    dots[[i]] = g1
  }
  
  dots
}

dots3 = dot(rf6.mx, rf6.mn, rf6.precip, sset = set3, setnum = 3)


png(file = "../images/varImpTest.png", height = 15, width = 10, units = "in", res = 300)
grid.draw(rbind(ggplotGrob(dots3[[6]]),
                ggplotGrob(dots3[[5]]),
                ggplotGrob(dots3[[4]]),
                ggplotGrob(dots3[[3]]),
                ggplotGrob(dots3[[2]]),
                ggplotGrob(dots3[[1]]),
                size = "last"))
dev.off()









