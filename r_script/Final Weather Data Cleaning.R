# Set working directory to source file location prior to running. 

library(dplyr)
library(lubridate)
library(tidyr)

weather = read.csv("../Data/weather2.csv")
head(weather)
colnames(weather)
summary(weather)

weather$Max_TemperatureF[weather$Max_TemperatureF > 200] = NA
weather$Min_TemperatureF[weather$Min_TemperatureF < -100] = NA
weather$Mean_TemperatureF[weather$Mean_TemperatureF < -100] = NA
summary(weather)

par(mfrow = c(2,2))
hist(weather$Mean_TemperatureF[weather$state == "Hawaii"])
hist(weather$Min_TemperatureF[weather$state == "Hawaii"])
hist(weather$Mean_TemperatureF[weather$state == "Florida"])
hist(weather$Min_TemperatureF[weather$state == "Florida"])

# Two names in this file do not match the names in the final locations file.
# St. George and Havre are different. This bit makes them match the final 
# locations file. 
weather$city = as.character(weather$city)
weather$city[weather$city == "St. George"] = "St George"
weather$city[weather$city == "Harve"] = "Havre"


write.csv(weather, "../Data/final_weather.csv", quote = FALSE, row.names = FALSE)

weather$Date = as.Date(as.character(weather$Date), format = "%Y-%m-%d")
sum(weather$city == "Denver")
