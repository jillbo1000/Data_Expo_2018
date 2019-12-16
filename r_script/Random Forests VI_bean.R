# Set working directory to source file location prior to running. 

library(lubridate)
library(tidyverse)
library(randomForest)
library(gridExtra)
library(RColorBrewer)

# Read in weather, forecast, and city data. 
rf.dat = read.csv("../Data/Combined_Data_byLag_Month.csv")
summary(rf.dat)
colnames(rf.dat)

clusters = read.csv("../Data/Cluster_Summary.csv")
clusters = select(clusters,BSS, Cluster5, Cluster6, Cluster7, Cluster8, AirPtCd)

rf.dat = left_join(rf.dat, clusters)
rf.dat = select(rf.dat, -mxT_mean_diff, -mxT_sd_diff, -mnT_mean_diff, -mnT_sd_diff, 
                -state, -city, -name.arpt, -state.arpt, -Max_Gust, -lon.arpt, 
                -lat.arpt, -elev.arpt, -WMO, -census2010, -est2010, 
                -est2016, -coastRegion, -coast.arpt, -coastRegion.arpt, 
                -latitude, -longitude)

# The following code looks for missing values. Baltimore and Austin are missing 
# for several of the variables. When we remove the rows with NAs I believe that
# all of the observations for both of these weather stations are removed. 
# There are also a ton of rows missing the min temperature. We
# should probably do some imputation rather than omit all of the NA 
# observations.
unique(rf.dat$city[is.na(rf.dat$mxT_mean)])
unique(rf.dat$city[is.na(rf.dat$mxT_sd)])
unique(rf.dat$city[is.na(rf.dat$mnT_mean)])
unique(rf.dat$city[is.na(rf.dat$mnT_sd)])
unique(rf.dat$city[is.na(rf.dat$Mean_Vis)])
unique(rf.dat$city[is.na(rf.dat$Min_Vis)])
unique(rf.dat$city[is.na(rf.dat$CloudCover)])
unique(rf.dat$city[is.na(rf.dat$Max_Wind_Speed)])
unique(rf.dat$city[is.na(rf.dat$Mean_Wind_Speed )])
unique(rf.dat$city[is.na(rf.dat$Max_Gust)])

###############################################################################
#
#                           Variable Importance
#
###############################################################################

rf.impute = read.csv("../Data/Imputed Random Forest Data.csv")
rf.impute$AirPtCd = rf.dat$AirPtCd
rf.impute = rf.impute[, c(36, 2:35)]

# Newer data did not need any imputed data
colnames(rf.impute)
predictors = rf.impute[, -c(1, 8:13, 31:35)]
head(predictors)

response = rf.impute[, c(8:13, 31)]
head(response)

rf6.mx = vector("list", 6)
rf6.mn = vector("list", 6)
rf6.precip = vector("list", 6)
for(i in 1:6) {
  tmp.pred = predictors[rf.impute$Cluster6 == i, ]
  tmp.res = response[rf.impute$Cluster6 == i, ]
  rf6.mx[[i]] = randomForest(tmp.pred, tmp.res$mxT_mean_abs, keep.forest = TRUE, importance = TRUE)
  rf6.mn[[i]] = randomForest(tmp.pred, tmp.res$mnT_mean_abs, keep.forest = TRUE, importance = TRUE)
  rf6.precip[[i]] = randomForest(tmp.pred, tmp.res$BSS, keep.forest = TRUE, importance = TRUE)
}

mx.mat <- matrix(0, nrow = nrow(rf6.mx[[1]]$importance), ncol = 6)
mn.mat <- matrix(0, nrow = nrow(rf6.mx[[1]]$importance), ncol = 6)
precip.mat <- matrix(0, nrow = nrow(rf6.mx[[1]]$importance), ncol = 6)
for(i in 1:6){
  mx.mat[, i] <- rf6.mx[[i]]$importance[, 1] 
  mn.mat[, i] <- rf6.mn[[i]]$importance[, 1] 
  precip.mat[, i] <- rf6.precip[[i]]$importance[, 1] 
}

pdf("../images/SampleHeatMaps.pdf")
heatmap(mx.mat, Rowv = NA, Colv = "Rowv",
        col = brewer.pal(9, "Reds"), main = "Max Temp")
heatmap(mn.mat, Rowv = NA, Colv = "Rowv",
        col = brewer.pal(9, "Reds"), main = "Min Temp")
heatmap(precip.mat, Rowv = NA, Colv = "Rowv",
        col = brewer.pal(9, "Reds"), main = "Precip")
dev.off()
















