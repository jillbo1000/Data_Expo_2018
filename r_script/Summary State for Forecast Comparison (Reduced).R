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

# Used this line to match Jill's syntax
combSum <- comb

# Some of the lags are NA so they are removed
combSum = combSum[!is.na(combSum$lag), ]

# 29 of the airports have 7 day forecasts
as.data.frame(combSum[combSum$lag == 7, ])

# Combine weather accuracy data with city information
acc = full_join(combSum, city)
acc$month <- month(acc$Date)


head(acc)
colnames(acc)

acc.sub <- acc %>%
  dplyr::select(AirPtCd, month, lag, maxTmp_diff, minTmp_diff) %>%
  group_by(AirPtCd, month, lag) %>%
  summarize(maxTmp_diff = mean(abs(maxTmp_diff), na.rm = TRUE),
            minTmp_diff = mean(abs(minTmp_diff), na.rm = TRUE))
write.csv(acc.sub, file =  "../Data/ForecastReduced2.csv")

# acc.sub <- acc %>% 
#   dplyr::select(AirPtCd, Date, lag, maxTmp_diff, minTmp_diff)
# write.csv(acc.sub, file =  "../Data/ForecastReduced2.csv")

# write.csv(acc.sub, file =  "../Data/ForecastReduced.csv")
