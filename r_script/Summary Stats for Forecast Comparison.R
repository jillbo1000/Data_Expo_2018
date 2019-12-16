setwd("C:/Users/jflun/Dropbox/USU/Advanced Graphics II/Data Expo/DataExpo2018")

library(dplyr)
library(lubridate)
library(tidyr)

# Read in weather, forecast, and city data. 
city = read.csv("../Data/locationsFinal.csv")
head(city)
colnames(city)
summary(city)
city$city = as.character(city$city)

weather = read.csv("../Data/final_weather.csv")
head(weather)
weather$Date = as.Date(as.character(weather$Date), format = "%Y-%m-%d")
# weather$city2 = city$city[match(weather$city, rownames(city))]
colnames(weather)
summary(weather)
weather$city = as.character(weather$city)

forecast = read.csv("../Data/final_forecast.csv")
head(forecast)
forecast$fdate = as.Date(as.character(forecast$fdate), format = "%Y-%m-%d")
forecast$date = as.Date(as.character(forecast$date), format = "%Y-%m-%d")
forecast$AirPtCd = city$AirPtCd[match(forecast$city, rownames(city))]
colnames(forecast)
summary(forecast)

forecast2 = forecast[, c(9, 2, 4:8)]
colnames(forecast2) = c("AirPtCd", "Date", "F_maxTmp", "F_minTmp", 
                        "F_PrecipAM", "F_PrecipPM", "lag")
head(forecast2)

# Pair up forecast and weather data
comb = full_join(forecast2, weather[, c(1, 2, 5, 6, 7, 9, 25)])
head(comb)

comb$maxTmp_diff = comb$F_maxTmp - comb$Max_TemperatureF
comb$minTmp_diff = comb$F_minTmp - comb$Min_TemperatureF
# Based on previous data analysis I chose a probability of 
# precipitation to be a prediction of rain.
comb$PrecipF = ifelse((comb$F_PrecipAM) > 20, 1, 0)
comb$Precip_Actual = ifelse(comb$PrecipitationIn > 0, 1, 0)
comb$Precip_Pred = ifelse(comb$PrecipF == comb$Precip_Actual, 1, 0)
head(comb)
summary(comb)

combSum = group_by(comb, AirPtCd, lag) %>%
  select(city, lag, maxTmp_diff, minTmp_diff, Precip_Pred) %>%
  summarise(mxT_mean = mean(maxTmp_diff, na.rm = TRUE), 
            mxT_sd = sd(maxTmp_diff, na.rm = TRUE), 
            mnT_mean = mean(minTmp_diff, na.rm = TRUE), 
            mnT_sd = sd(minTmp_diff, na.rm = TRUE), 
            mxT_mean_abs = mean(abs(maxTmp_diff), na.rm = TRUE), 
            mxT_sd_abs = sd(abs(maxTmp_diff), na.rm = TRUE), 
            mnT_mean_abs = mean(abs(minTmp_diff), na.rm = TRUE), 
            mnT_sd_abs = sd(abs(minTmp_diff), na.rm = TRUE), 
            Precip_Acc = mean(Precip_Pred, na.rm = TRUE))

head(combSum, 20)

# Use this line if you want ALL the days, and not a summary
# combSum <- comb

# Some of the lags are NA so they are removed
combSum = combSum[!is.na(combSum$lag), ]

# 29 of the airports have 7 day forecasts
as.data.frame(combSum[combSum$lag == 7, ])

# Combine weather accuracy data with city information
acc = full_join(combSum, city)
head(acc)
colnames(acc)

acc.sub <- acc %>% dplyr::select(AirPtCd, Date, lag, maxTmp_diff, minTmp_diff)

write.csv(acc.sub, file =  "../Data/ForecastReduced.csv")

# Make maps with demographics
library(ggmap)
library(sp)
library(rgdal)
library(gstat)
library(RgoogleMaps)
library(PBSmapping)
library(maptools)
library(gridExtra)

# Initial test map
qmplot(longitude, latitude, data = acc)

# Add max temp accuracy
hist(acc$mxT_mean)
qmplot(longitude, latitude, data = acc, size = mxT_mean,
       #zoom = 14, source = "google", maptype = "satellite", 
       alpha = I(.75), color = I("green"),
       legend = "topright") + 
  scale_size("Max Temp (F)") +
  facet_wrap(~lag)

qmplot(longitude, latitude, data = acc, legend = "bottomright") +
  geom_point(aes(size = mxT_mean),
             shape = I(19),
             data = acc, color = elevation) +
  scale_size("Max Temp (F)") +
  facet_wrap(~lag)

bb <- qbbox(lat = acc[, "latitude"], lon = acc[, "longitude"])
bb



us = get_map(location = c(bb$lon[1], bb$latR[2], bb$lonR[2], bb$latR[1]), 
             source = "osm", maptype = "toner", crop = TRUE)
us = get_map(zoom = 3, source = "stamen", maptype = "toner-2010")

# Graphs for elevation
# This code can be altered to produce the graphs for any temperature measure.
# To graph the color as a red/blue color scheme change type to "div", 
# palette to "RdBu", and direction to -1. To have a red sequential color
# scheme change type to "seq", palette to "Reds", and direction to 1 or -1
# depending on the direction you want for your variable. Change the other
# names and variables as needed.

us = get_map(zoom = 3, source = "stamen", maptype = "toner-2010")

graphs = vector("list", 7)
for(i in 1:7) {
  tmp = acc[acc$lag == (i - 1), ]
  graphs[[i]] = ggmap(us) +
    geom_point(aes(x = longitude, y = latitude, color = round(mnT_sd), size = elevation), 
               data = tmp) + 
    expand_limits(x = bb$lonR, y = bb$latR) +
    scale_color_distiller(name = "Min Temp SD", type = "seq",
                          palette = "Reds", direction = 1) +
    scale_size(name = "Elevation", breaks = seq(0, 2500, 200))
}  
  
graphs[[1]] + ggtitle("Lag 0")
graphs[[2]] + ggtitle("Lag 1")
graphs[[3]] + ggtitle("Lag 2")
graphs[[4]] + ggtitle("Lag 3")
graphs[[5]] + ggtitle("Lag 4")
graphs[[6]] + ggtitle("Lag 5")
graphs[[7]] + ggtitle("Lag 6")

# This time I did temp diff with size and elevation by color. This made more sense
us = get_map(zoom = 3, source = "stamen", maptype = "toner-2010")

graphs2 = vector("list", 7)
for(i in 1:7) {
  tmp = acc[acc$lag == (i - 1), ]
  graphs2[[i]] = ggmap(us) +
    geom_point(aes(x = longitude, y = latitude, color = elevation, size = mxT_mean_abs), 
               data = tmp) + 
    expand_limits(x = bb$lonR, y = bb$latR) +
    scale_color_distiller(name = "Elevation", type = "seq",
                          palette = "Greens", direction = 1) +
    scale_size(name = "Max Temp", breaks = seq(0, 10, 2))
}  

graphs2[[1]] + ggtitle("Lag 0")
graphs2[[2]] + ggtitle("Lag 1")
graphs2[[3]] + ggtitle("Lag 2")
graphs2[[4]] + ggtitle("Lag 3")
graphs2[[5]] + ggtitle("Lag 4")
graphs2[[6]] + ggtitle("Lag 5")
graphs2[[7]] + ggtitle("Lag 6")

# I removed the covariate this time
us = get_map(zoom = 3, source = "google", maptype = "terrain", color = "bw")
ggmap(us)

graphs.mxT = vector("list", 7)
for(i in 1:7) {
  tmp = acc[acc$lag == (i - 1), ]
  graphs.mxT[[i]] = ggmap(us) +
    geom_point(aes(x = longitude, y = latitude, color = round(mxT_mean_abs)), 
               data = tmp) + 
    expand_limits(colour = seq(0, 10, 2)) +
    xlim(-123.9, -66.99) +
    ylim(24.55, 48.55) +
    scale_color_distiller(name = "Max Temp", type = "seq",
                          palette = "Reds", direction = 1, 
                          guide = "legend", values = seq(0, 10, 2)/10)
}  

graphs.mnT = vector("list", 7)
for(i in 1:7) {
  tmp = acc[acc$lag == (i - 1), ]
  graphs.mnT[[i]] = ggmap(us) +
    geom_point(aes(x = longitude, y = latitude, color = round(mnT_mean_abs)), 
               data = tmp) + 
    expand_limits(colour = seq(0, 10, 2)) +
    xlim(-123.9, -66.99) +
    ylim(24.55, 48.55) +
    scale_color_distiller(name = "Min Temp", type = "seq",
                          palette = "Reds", direction = 1, 
                          guide = "legend", values = seq(0, 10, 2)/10)
}  

graphs.Pcp = vector("list", 7)
for(i in 1:7) {
  tmp = acc[acc$lag == (i - 1), ]
  graphs.Pcp[[i]] = ggmap(us) +
    geom_point(aes(x = longitude, y = latitude, color = Precip_Acc), 
               data = tmp) + 
    expand_limits(colour = seq(0, 1, 0.25)) +
    xlim(-123.9, -66.99) +
    ylim(24.55, 48.55) +
    scale_color_distiller(name = "Precipitation", type = "seq",
                          palette = "Reds", direction = -1, 
                          guide = "legend", values = seq(0, 1, 0.25))
}  


pdf("Preliminary Data Assessment/Prediction Errors.pdf", height = 9, width = 6.5)
g1 <- ggplotGrob(graphs.mxT[[1]])
id.legend <- grep("guide", g1$layout$name)
legend <- g1[["grobs"]][[id.legend]]
lwidth <- sum(legend$width)

grid.arrange(graphs.mxT[[7]] + theme(legend.position="none") + ggtitle("Lag 6"), 
             graphs.mxT[[6]] + theme(legend.position="none") + ggtitle("Lag 5"), 
             graphs.mxT[[5]] + theme(legend.position="none") + ggtitle("Lag 4"), 
             graphs.mxT[[4]] + theme(legend.position="none") + ggtitle("Lag 3"), 
             graphs.mxT[[3]] + theme(legend.position="none") + ggtitle("Lag 2"), 
             graphs.mxT[[2]] + theme(legend.position="none") + ggtitle("Lag 1"), 
             graphs.mxT[[1]] + theme(legend.position="none") + ggtitle("Lag 0"), 
             legend, 
             layout_matrix = rbind(c(1,1,2,2,8), 
                                   c(3,3,4,4,8),
                                   c(5,5,6,6,8),
                                   c(7,7,NA,NA,8)), 
             top = "Maximum Temperature")

grid.arrange(graphs.mnT[[7]] + theme(legend.position="none") + ggtitle("Lag 6"), 
             graphs.mnT[[6]] + theme(legend.position="none") + ggtitle("Lag 5"), 
             graphs.mnT[[5]] + theme(legend.position="none") + ggtitle("Lag 4"), 
             graphs.mnT[[4]] + theme(legend.position="none") + ggtitle("Lag 3"), 
             graphs.mnT[[3]] + theme(legend.position="none") + ggtitle("Lag 2"), 
             graphs.mnT[[2]] + theme(legend.position="none") + ggtitle("Lag 1"), 
             graphs.mnT[[1]] + theme(legend.position="none") + ggtitle("Lag 0"), 
             legend, 
             layout_matrix = rbind(c(1,1,2,2,8), 
                                   c(3,3,4,4,8),
                                   c(5,5,6,6,8),
                                   c(7,7,NA,NA,8)), 
             top = "Minimum Temperature")

g1 <- ggplotGrob(graphs.Pcp[[1]])
id.legend <- grep("guide", g1$layout$name)
legend <- g1[["grobs"]][[id.legend]]
lwidth <- sum(legend$width)

grid.arrange(graphs.Pcp[[7]] + theme(legend.position="none") + ggtitle("Lag 6"), 
             graphs.Pcp[[6]] + theme(legend.position="none") + ggtitle("Lag 5"), 
             graphs.Pcp[[5]] + theme(legend.position="none") + ggtitle("Lag 4"), 
             graphs.Pcp[[4]] + theme(legend.position="none") + ggtitle("Lag 3"), 
             graphs.Pcp[[3]] + theme(legend.position="none") + ggtitle("Lag 2"), 
             graphs.Pcp[[2]] + theme(legend.position="none") + ggtitle("Lag 1"), 
             graphs.Pcp[[1]] + theme(legend.position="none") + ggtitle("Lag 0"), 
             legend, 
             layout_matrix = rbind(c(1,1,2,2,8), 
                                   c(3,3,4,4,8),
                                   c(5,5,6,6,8),
                                   c(7,7,NA,NA,8)), 
             top = "Precipitation")
dev.off()


