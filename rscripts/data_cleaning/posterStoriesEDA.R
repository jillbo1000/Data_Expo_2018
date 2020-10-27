# Check and see if there is a distinct bias in Austin temp predictions
obs <- read.csv("../../data/final_weather.csv")
temp <- read.csv("../../data/summary_city_lag.csv")
locs <- read.csv("../../data/locationsFinal.csv")

temp.sub <- temp[temp$AirPtCd == "KP68", ]



# After running a portion of "final_data_summaries", 
# I wanted to see how bad minimum temperature predictions were 
# on the coldest days. 
obs.sub <- obs[!is.na(obs$Min_TemperatureF) & !is.na(obs$AirPtCd), ]
obs.sub <- obs.sub[obs.sub$Min_TemperatureF < -20, ]

checkit <- left_join(obs.sub, locs, by = "AirPtCd")

sum(checkit$Cluster6 == 2)/nrow(checkit)

mean(abs(comb$minTmp_diff[comb$Min_TemperatureF < 32]), na.rm = TRUE)
mean(abs(comb$minTmp_diff[comb$Min_TemperatureF >= 32]), na.rm = TRUE)

# Check and see which categories Key West, FL has the lowest variability in...
tempSum <- read.csv("../../data/summary_city.csv")

tempSum.sub <- dplyr::select(tempSum, contains("Sd"))


for(i in 1:ncol(tempSum.sub)){
  print(head(as.character(tempSum$city[order(tempSum.sub[, i])])))
}
