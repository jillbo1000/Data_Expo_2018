library(dplyr)
library(lubridate)
library(tidyr)
library(randomForest)
library(mice)
library(VIM)
library(missForest)

# Read in weather, forecast, and city data. 
clusters = read.csv("../Data/Cluster_Summary.csv")
clusters = select(clusters, Cluster5, Cluster6, Cluster7, Cluster8, AirPtCd)

city = read.csv("../Data/locationsFinal.csv")
city = select(city, AirPtCd, city, state)

clust = left_join(city, clusters)

head(clust)

clust = clust[order(clust$Cluster8), ]
clust
