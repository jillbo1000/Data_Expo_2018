setwd("C:/Users/jflun/Dropbox/USU/Advanced Graphics II/Data Expo/DataExpo2018")

library(dplyr)
library(lubridate)
library(tidyr)
library(RColorBrewer)

# Read in weather, forecast, and city data. 
city = read.csv("Data/locationsFinal.csv")
head(city)
colnames(city)
summary(city)
city$city = as.character(city$city)

weather = read.csv("Data/final_weather.csv")
head(weather)
weather$Date = as.Date(as.character(weather$Date), format = "%Y-%m-%d")
# weather$city2 = city$city[match(weather$city, rownames(city))]
colnames(weather)
summary(weather)
weather$city = as.character(weather$city)

forecast = read.csv("Data/final_forecast.csv")
head(forecast)
forecast$fdate = as.Date(as.character(forecast$fdate), format = "%Y-%m-%d")
forecast$date = as.Date(as.character(forecast$date), format = "%Y-%m-%d")
forecast$city2 = city$city[match(forecast$city, rownames(city))]
colnames(forecast)
summary(forecast)

forecast2 = forecast[, c(9, 2, 4:8)]
colnames(forecast2) = c("city", "Date", "F_maxTmp", "F_minTmp", "F_PrecipAM", "F_PrecipPM", "lag")
head(forecast2)
forecast2$city = as.character(forecast2$city)

# Pair up forecast and weather data
comb = full_join(forecast2, weather[, c(1, 2, 6, 7, 9, 25)])
head(comb)

comb$maxTmp_diff = comb$F_maxTmp - comb$Max_TemperatureF
comb$minTmp_diff = comb$F_minTmp - comb$Min_TemperatureF
comb$PrecipF = ifelse((comb$F_PrecipAM) > 30, 1, 0)
comb$Precip_Actual = ifelse(comb$PrecipitationIn > 0, 1, 0)
comb$Precip_Pred = ifelse(comb$PrecipF == comb$Precip_Actual, 1, 0)
head(comb)
summary(comb)

lagdat = vector("list", 7)
for(i in 1:7) {
  lagdat[[i]] = comb[comb$lag == (i - 1), ]
}

# AM accuracy, specificity, and sensitivity
precip_accAM = matrix(0, ncol = 7, nrow = 11)
rownames(precip_accAM) = seq(0, 1, 0.1)
colnames(precip_accAM) = paste("Day", 0:6, sep = "")

for(i in 1:7) {
  tmp = lagdat[[i]]
  for(j in 1:11) {
    tmp1 = ifelse(tmp$F_PrecipAM <= as.numeric(rownames(precip_accAM)[j]) * 100, 0, 1)
    tmp2 = ifelse(tmp$PrecipitationIn > 0, 1, 0)
    precip_accAM[j, i] = mean(tmp1 == tmp2, na.rm = TRUE)
  }
}
precip_accAM

precip_senAM = matrix(0, ncol = 7, nrow = 11)
rownames(precip_senAM) = seq(0, 1, 0.1)
colnames(precip_senAM) = paste("Day", 0:6, sep = "")

for(i in 1:7) {
  tmp = lagdat[[i]]
  for(j in 1:11) {
    tmp1 = ifelse(tmp$F_PrecipAM <= as.numeric(rownames(precip_senAM)[j]) * 100, 0, 1)
    tmp2 = ifelse(tmp$PrecipitationIn > 0, 1, 0)
    tmp1a = tmp2[tmp1 == 1]
    precip_senAM[j, i] = mean(tmp1a, na.rm = TRUE)
  }
}
precip_senAM

precip_spAM = matrix(0, ncol = 7, nrow = 11)
rownames(precip_spAM) = seq(0, 1, 0.1)
colnames(precip_spAM) = paste("Day", 0:6, sep = "")

for(i in 1:7) {
  tmp = lagdat[[i]]
  for(j in 1:11) {
    tmp1 = ifelse(tmp$F_PrecipAM <= as.numeric(rownames(precip_spAM)[j]) * 100, 0, 1)
    tmp2 = ifelse(tmp$PrecipitationIn > 0, 1, 0)
    tmp1a = !tmp2[tmp1 == 0]
    precip_spAM[j, i] = mean(tmp1a, na.rm = TRUE)
  }
}
precip_spAM

# PM accuracy, specificity, and sensitivity
precip_accPM = matrix(0, ncol = 7, nrow = 11)
rownames(precip_accPM) = seq(0, 1, 0.1)
colnames(precip_accPM) = paste("Day", 0:6, sep = "")

for(i in 1:7) {
  tmp = lagdat[[i]]
  for(j in 1:11) {
    tmp1 = ifelse(tmp$F_PrecipPM <= as.numeric(rownames(precip_accPM)[j]) * 100, 0, 1)
    tmp2 = ifelse(tmp$PrecipitationIn > 0, 1, 0)
    precip_accPM[j, i] = mean(tmp1 == tmp2, na.rm = TRUE)
  }
}
precip_accPM

precip_senPM = matrix(0, ncol = 7, nrow = 11)
rownames(precip_senPM) = seq(0, 1, 0.1)
colnames(precip_senPM) = paste("Day", 0:6, sep = "")

for(i in 1:7) {
  tmp = lagdat[[i]]
  for(j in 1:11) {
    tmp1 = ifelse(tmp$F_PrecipPM <= as.numeric(rownames(precip_senPM)[j]) * 100, 0, 1)
    tmp2 = ifelse(tmp$PrecipitationIn > 0, 1, 0)
    tmp1a = tmp2[tmp1 == 1]
    precip_senPM[j, i] = mean(tmp1a, na.rm = TRUE)
  }
}
precip_senPM

precip_spPM = matrix(0, ncol = 7, nrow = 11)
rownames(precip_spPM) = seq(0, 1, 0.1)
colnames(precip_spPM) = paste("Day", 0:6, sep = "")

for(i in 1:7) {
  tmp = lagdat[[i]]
  for(j in 1:11) {
    tmp1 = ifelse(tmp$F_PrecipPM <= as.numeric(rownames(precip_spPM)[j]) * 100, 0, 1)
    tmp2 = ifelse(tmp$PrecipitationIn > 0, 1, 0)
    tmp1a = !tmp2[tmp1 == 0]
    precip_spPM[j, i] = mean(tmp1a, na.rm = TRUE)
  }
}
precip_spPM

# Plots to Explore Sensitivity/Specificity
par(mfrow = c(2, 2))
plot(as.numeric(rownames(precip_spPM)), precip_senAM[, 6], pch = "", 
     main = "Sen/Sp Precip AM", xlab = "Pred Prob of Precip")
for(i in 1:7) {
  lines(as.numeric(rownames(precip_spPM)), precip_senAM[, i], col = "dodgerblue")
  lines(as.numeric(rownames(precip_spPM)), precip_spAM[, i], col = "mediumorchid1")
}

plot(as.numeric(rownames(precip_spPM)), precip_senPM[, 6], pch = "", 
     main = "Sen/Sp Precip PM", xlab = "Pred Prob of Precip")
for(i in 1:7) {
  lines(as.numeric(rownames(precip_spPM)), precip_senPM[, i], col = "dodgerblue")
  lines(as.numeric(rownames(precip_spPM)), precip_spPM[, i], col = "mediumorchid1")
}

plot(as.numeric(rownames(precip_spPM)), precip_senPM[, 6], pch = "", 
     main = "Sen Precip AM/PM", xlab = "Pred Prob of Precip")
for(i in 1:7) {
  lines(as.numeric(rownames(precip_spPM)), precip_senPM[, i], col = "darkolivegreen4")
  lines(as.numeric(rownames(precip_spPM)), precip_senAM[, i], col = "deeppink3")
}

plot(as.numeric(rownames(precip_spPM)), precip_senPM[, 6], pch = "", 
     main = "Spec Precip AM/PM", xlab = "Pred Prob of Precip")
for(i in 1:7) {
  lines(as.numeric(rownames(precip_spPM)), precip_senPM[, i], col = "darkolivegreen4")
  lines(as.numeric(rownames(precip_spPM)), precip_senAM[, i], col = "deeppink3")
}

# Markers changed by lag
par(mfrow = c(2, 2))
colors = brewer.pal(7, "Dark2")

plot(as.numeric(rownames(precip_spPM)), precip_senAM[, 6], pch = "", 
     main = "Sen/Sp Precip AM", xlab = "Pred Prob of Precip")
for(i in 1:7) {
  lines(as.numeric(rownames(precip_spPM)), precip_senAM[, i], col = colors[i])
  lines(as.numeric(rownames(precip_spPM)), precip_spAM[, i], col = colors[i])
}
legend("topright", legend = 0:6, col = colors, fill = colors, cex = 0.5)

plot(as.numeric(rownames(precip_spPM)), precip_senPM[, 6], pch = "", 
     main = "Sen/Sp Precip PM", xlab = "Pred Prob of Precip")
for(i in 1:7) {
  lines(as.numeric(rownames(precip_spPM)), precip_senPM[, i], col = colors[i])
  lines(as.numeric(rownames(precip_spPM)), precip_spPM[, i], col = colors[i])
}
legend("topright", legend = 0:6, fill = colors, col = colors, cex = 0.5)

plot(as.numeric(rownames(precip_spPM)), precip_senPM[, 6], pch = "", 
     main = "Sen Precip AM/PM", xlab = "Pred Prob of Precip")
for(i in 1:7) {
  lines(as.numeric(rownames(precip_spPM)), precip_senPM[, i], col = "#1b9e77")
  lines(as.numeric(rownames(precip_spPM)), precip_senAM[, i], col = "#d95f02")
}
legend("topright", legend = c("AM", "PM"), fill = c("#d95f02", "#1b9e77"), cex = 0.5)

plot(as.numeric(rownames(precip_spPM)), precip_senPM[, 6], pch = "", 
     main = "Spec Precip AM/PM", xlab = "Pred Prob of Precip")
for(i in 1:7) {
  lines(as.numeric(rownames(precip_spPM)), precip_spPM[, i], col = "#1b9e77")
  lines(as.numeric(rownames(precip_spPM)), precip_spAM[, i], col = "#d95f02")
}
legend("topright", legend = c("AM", "PM"), fill = c("#d95f02", "#1b9e77"), cex = 0.5)




