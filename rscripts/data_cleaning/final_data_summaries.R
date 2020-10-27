# Final Data Aggregation Script
# Created: 6-22-2018
# Last Modified: 10-23-2020
# Description:
#   There are multiple versions of the data aggregation files. This script
#   consolidates all of those versions into one common file. The result
#   is three separate datasets, all of which group on a different set of 
#   variables. 
#=============================================================================
# Set working directory to source file location prior to running. 

# Step 1 - Load the cleaned versions of the data files. 
#=============================================================================
library(tidyverse)
library(lubridate)

# Read in weather, forecast, and city data. 
city <- read.csv("../../data/locationsFinal_preclust.csv") %>%
  dplyr::select(-name.arpt, -state.arpt, -lon.arpt, -lat.arpt, -elev.arpt,
                -WMO, -census2010, -est2010, -est2016, -coastRegion)
city$city <- as.character(city$city)

weather <- read.csv("../../data/final_weather.csv") %>% 
  dplyr::select(-Mean_TemperatureF, -MeanDew_PointF, -Mean_Humidity,
                -Mean_Sea_Level_PressureIn, -Max_VisibilityMiles,
                -Mean_VisibilityMiles, -Max_Gust_SpeedMPH, -WindDirDegrees)
weather$Date <- as.Date(as.character(weather$Date), format = "%Y-%m-%d")
weather$month <- month(weather$Date)

# Determine whether or not there was a storm that day that produced precipitation. 
weather$Events[is.na(weather$Events)] <- "" # Replace missing events with a blank description. 
weather$RainEVNT <- regexpr(pattern = "[Rr][Aa][Ii][Nn]", text = weather$Events) > 0
weather$SnowEVNT <- regexpr(pattern = "[Ss][Nn][Oo][Ww]", text = weather$Events) > 0
weather$PrecipEVNT <- weather$RainEVNT | weather$SnowEVNT
weather$Precip_Actual <- weather$PrecipitationIn > 0 | weather$PrecipEVNT
# Even if the rain value is missing, record precip as true is a storm event was recorded. 
weather$Precip_Actual[is.na(weather$PrecipitationIn) & weather$PrecipEVNT] <- TRUE
weather <- dplyr::select(weather, -Events, -RainEVNT, -SnowEVNT, -PrecipEVNT)

# Now, determine the overall proportion of rainy days for each station location over the
# period of record. 
ovProp <- weather %>% 
  group_by(AirPtCd, Date) %>% 
  slice(1) %>% # Only look at one observation per day to calculate proportions. 
  group_by(AirPtCd) %>%
  summarize(RD = sum(Precip_Actual, na.rm = TRUE),
            TD = sum(!is.na(Precip_Actual))) %>%
  mutate(prop = RD/TD) %>%
  dplyr::select(-RD, -TD)

weather <- left_join(weather, ovProp, by = "AirPtCd")

forecast <- read.csv("../../data/final_forecast.csv")
head(forecast)
forecast$fdate <- as.Date(as.character(forecast$fdate), format = "%Y-%m-%d")
forecast$date <- as.Date(as.character(forecast$date), format = "%Y-%m-%d")
forecast$AirPtCd <- city$AirPtCd[match(forecast$city, rownames(city))]
colnames(forecast)
summary(forecast)

forecast2 <- forecast[, c(9, 2, 4:8)]
colnames(forecast2) <- c("AirPtCd", "Date", "F_maxTmp", "F_minTmp", 
                        "F_PrecipAM", "F_PrecipPM", "lag")

forecast2$F_PrecipMAX <- forecast2$F_PrecipAM
# Because we only want the max and ONLY PM precipitations are missing, replace
# all missing PM precipitation values with 0. At worst, we will stick with the 
# AM precipitation. 
forecast2$F_PrecipPM[is.na(forecast2$F_PrecipPM)] <- 0
forecast2$F_PrecipMAX[forecast2$F_PrecipAM < forecast2$F_PrecipPM] <- 
  forecast2$F_PrecipPM[forecast2$F_PrecipAM < forecast2$F_PrecipPM]
forecast2$F_PrecipAM <- NULL
forecast2$F_PrecipPM <- NULL
#=============================================================================

# Step 2 - Combine information and calculate accuracy
#=============================================================================
# Calculate the forecast accuray for the temperature measurements. 
comb <- full_join(forecast2, weather, by = c("AirPtCd", "Date"))
head(comb)

comb$maxTmp_diff <- comb$F_maxTmp - comb$Max_TemperatureF
comb$minTmp_diff <- comb$F_minTmp - comb$Min_TemperatureF

# Calculate the Brier Skill Score 
comb$BrS <- ((comb$F_PrecipMAX/100) - as.numeric(comb$Precip_Actual))^2 
comb$BrSC <- (comb$prop - as.numeric(comb$Precip_Actual))^2 
#=============================================================================

# Step 3.1 - Aggregate results by airport, month, and lag. 
#=============================================================================
# Aggregate by airport, month, and lag. 
combSum1 <- comb %>% group_by(AirPtCd, month, lag) %>%
  summarise(
    Sd_Max_Temp = sd(Max_TemperatureF, na.rm = TRUE),
    Max_Temp = mean(Max_TemperatureF, na.rm = TRUE),
    Sd_Min_Temp = sd(Min_TemperatureF, na.rm = TRUE),
    Min_Temp = mean(Min_TemperatureF, na.rm = TRUE),
    Sd_Precip = sd(PrecipitationIn, na.rm = TRUE),
    Mean_Precip = mean(PrecipitationIn, na.rm = TRUE), 
    Sd_Max_DewPoint = sd(Max_Dew_PointF, na.rm = TRUE),
    Max_DewPoint = mean(Max_Dew_PointF, na.rm = TRUE), 
    Sd_Min_DewPoint = sd(Min_DewpointF, na.rm = TRUE),
    Min_DewPoint = mean(Min_DewpointF, na.rm = TRUE),
    Sd_Max_Humidity = sd(Max_Humidity, na.rm = TRUE),
    Max_Humidity = mean(Max_Humidity, na.rm = TRUE), 
    Sd_Min_Humidity = sd(Min_Humidity, na.rm = TRUE),
    Min_Humidity = mean(Min_Humidity, na.rm = TRUE),
    Sd_Max_Sea_Level_Press = sd(Max_Sea_Level_PressureIn, na.rm = TRUE),
    Max_Sea_Level_Press = mean(Max_Sea_Level_PressureIn, na.rm = TRUE), 
    Sd_Min_Sea_Level_Press = sd(Min_Sea_Level_PressureIn, na.rm = TRUE),
    Min_Sea_Level_Press = mean(Min_Sea_Level_PressureIn, na.rm = TRUE), 
    Sd_Vis = sd(Min_VisibilityMiles, na.rm = TRUE),
    Min_Vis = mean(Min_VisibilityMiles, na.rm = TRUE), 
    Sd_CloudCover = sd(CloudCover, na.rm = TRUE),
    CloudCover = mean(CloudCover, na.rm = TRUE), 
    Sd_Mean_Wind_Speed = sd(Mean_Wind_SpeedMPH, na.rm = TRUE),
    Mean_Wind_Speed = mean(Mean_Wind_SpeedMPH, na.rm = TRUE), 
    Sd_Max_Wind_Speed = sd(Max_Wind_SpeedMPH, na.rm = TRUE),
    Max_Wind_Speed = mean(Max_Wind_SpeedMPH, na.rm = TRUE), 
    # Summarize error variables.
    mxT_sd = sd(maxTmp_diff, na.rm = TRUE), 
    mxT_mean = mean(maxTmp_diff, na.rm = TRUE), 
    mnT_sd = sd(minTmp_diff, na.rm = TRUE), 
    mnT_mean = mean(minTmp_diff, na.rm = TRUE), 
    mxT_sd_abs = sd(abs(maxTmp_diff), na.rm = TRUE), 
    mxT_mean_abs = mean(abs(maxTmp_diff), na.rm = TRUE), 
    mnT_sd_abs = sd(abs(minTmp_diff), na.rm = TRUE), 
    mnT_mean_abs = mean(abs(minTmp_diff), na.rm = TRUE), 
    BSN = sum(BrS, na.rm = TRUE)/sum(!is.na(BrS)),
    BSD = sum(BrSC, na.rm = TRUE)/sum(!is.na(BrSC))
  ) %>%
  mutate(BSS = 1 - BSN/BSD) %>%
  dplyr::select(-BSN, -BSD) %>%
  filter(!is.na(month), !is.na(lag)) %>%
  left_join(., city, by = "AirPtCd")

combSum1$mxT_sd[is.nan(combSum1$mxT_sd)] <- NA
combSum1$mxT_mean[is.nan(combSum1$mxT_mean)] <- NA
combSum1$mnT_sd[is.nan(combSum1$mnT_sd)] <- NA
combSum1$mnT_mean[is.nan(combSum1$mnT_mean)] <- NA
combSum1$mxT_sd_abs[is.nan(combSum1$mxT_sd_abs)] <- NA
combSum1$mxT_mean_abs[is.nan(combSum1$mxT_mean_abs)] <- NA
combSum1$mnT_sd_abs[is.nan(combSum1$mnT_sd_abs)] <- NA
combSum1$mnT_mean_abs[is.nan(combSum1$mnT_mean_abs)] <- NA
#=============================================================================

# Step 3.2 - Aggregate results by airport and month 
#=============================================================================
# Aggregate by airport and month 
combSum2 <- comb %>% group_by(AirPtCd, month) %>%
  summarise(
    Sd_Max_Temp = sd(Max_TemperatureF, na.rm = TRUE),
    Max_Temp = mean(Max_TemperatureF, na.rm = TRUE),
    Sd_Min_Temp = sd(Min_TemperatureF, na.rm = TRUE),
    Min_Temp = mean(Min_TemperatureF, na.rm = TRUE),
    Sd_Precip = sd(PrecipitationIn, na.rm = TRUE),
    Mean_Precip = mean(PrecipitationIn, na.rm = TRUE), 
    Sd_Max_DewPoint = sd(Max_Dew_PointF, na.rm = TRUE),
    Max_DewPoint = mean(Max_Dew_PointF, na.rm = TRUE), 
    Sd_Min_DewPoint = sd(Min_DewpointF, na.rm = TRUE),
    Min_DewPoint = mean(Min_DewpointF, na.rm = TRUE),
    Sd_Max_Humidity = sd(Max_Humidity, na.rm = TRUE),
    Max_Humidity = mean(Max_Humidity, na.rm = TRUE), 
    Sd_Min_Humidity = sd(Min_Humidity, na.rm = TRUE),
    Min_Humidity = mean(Min_Humidity, na.rm = TRUE),
    Sd_Max_Sea_Level_Press = sd(Max_Sea_Level_PressureIn, na.rm = TRUE),
    Max_Sea_Level_Press = mean(Max_Sea_Level_PressureIn, na.rm = TRUE), 
    Sd_Min_Sea_Level_Press = sd(Min_Sea_Level_PressureIn, na.rm = TRUE),
    Min_Sea_Level_Press = mean(Min_Sea_Level_PressureIn, na.rm = TRUE), 
    Sd_Vis = sd(Min_VisibilityMiles, na.rm = TRUE),
    Min_Vis = mean(Min_VisibilityMiles, na.rm = TRUE), 
    Sd_CloudCover = sd(CloudCover, na.rm = TRUE),
    CloudCover = mean(CloudCover, na.rm = TRUE), 
    Sd_Mean_Wind_Speed = sd(Mean_Wind_SpeedMPH, na.rm = TRUE),
    Mean_Wind_Speed = mean(Mean_Wind_SpeedMPH, na.rm = TRUE), 
    Sd_Max_Wind_Speed = sd(Max_Wind_SpeedMPH, na.rm = TRUE),
    Max_Wind_Speed = mean(Max_Wind_SpeedMPH, na.rm = TRUE), 
    # Summarize error variables.
    mxT_sd = sd(maxTmp_diff, na.rm = TRUE), 
    mxT_mean = mean(maxTmp_diff, na.rm = TRUE), 
    mnT_sd = sd(minTmp_diff, na.rm = TRUE), 
    mnT_mean = mean(minTmp_diff, na.rm = TRUE), 
    mxT_sd_abs = sd(abs(maxTmp_diff), na.rm = TRUE), 
    mxT_mean_abs = mean(abs(maxTmp_diff), na.rm = TRUE), 
    mnT_sd_abs = sd(abs(minTmp_diff), na.rm = TRUE), 
    mnT_mean_abs = mean(abs(minTmp_diff), na.rm = TRUE), 
    BSN = sum(BrS, na.rm = TRUE)/sum(!is.na(BrS)),
    BSD = sum(BrSC, na.rm = TRUE)/sum(!is.na(BrSC))
  ) %>%
  mutate(BSS = 1 - BSN/BSD) %>%
  dplyr::select(-BSN, -BSD) %>%
  filter(!is.na(month)) %>%
  left_join(., city, by = "AirPtCd")

combSum2$mxT_sd[is.nan(combSum2$mxT_sd)] <- NA
combSum2$mxT_mean[is.nan(combSum2$mxT_mean)] <- NA
combSum2$mnT_sd[is.nan(combSum2$mnT_sd)] <- NA
combSum2$mnT_mean[is.nan(combSum2$mnT_mean)] <- NA
combSum2$mxT_sd_abs[is.nan(combSum2$mxT_sd_abs)] <- NA
combSum2$mxT_mean_abs[is.nan(combSum2$mxT_mean_abs)] <- NA
combSum2$mnT_sd_abs[is.nan(combSum2$mnT_sd_abs)] <- NA
combSum2$mnT_mean_abs[is.nan(combSum2$mnT_mean_abs)] <- NA
#=============================================================================

# Step 3.3 - Aggregate results by airport and lag
#=============================================================================
# Aggregate by airport and lag. 
combSum3 <- comb %>% group_by(AirPtCd, lag) %>%
  summarise(
    Sd_Max_Temp = sd(Max_TemperatureF, na.rm = TRUE),
    Max_Temp = mean(Max_TemperatureF, na.rm = TRUE),
    Sd_Min_Temp = sd(Min_TemperatureF, na.rm = TRUE),
    Min_Temp = mean(Min_TemperatureF, na.rm = TRUE),
    Sd_Precip = sd(PrecipitationIn, na.rm = TRUE),
    Mean_Precip = mean(PrecipitationIn, na.rm = TRUE), 
    Sd_Max_DewPoint = sd(Max_Dew_PointF, na.rm = TRUE),
    Max_DewPoint = mean(Max_Dew_PointF, na.rm = TRUE), 
    Sd_Min_DewPoint = sd(Min_DewpointF, na.rm = TRUE),
    Min_DewPoint = mean(Min_DewpointF, na.rm = TRUE),
    Sd_Max_Humidity = sd(Max_Humidity, na.rm = TRUE),
    Max_Humidity = mean(Max_Humidity, na.rm = TRUE), 
    Sd_Min_Humidity = sd(Min_Humidity, na.rm = TRUE),
    Min_Humidity = mean(Min_Humidity, na.rm = TRUE),
    Sd_Max_Sea_Level_Press = sd(Max_Sea_Level_PressureIn, na.rm = TRUE),
    Max_Sea_Level_Press = mean(Max_Sea_Level_PressureIn, na.rm = TRUE), 
    Sd_Min_Sea_Level_Press = sd(Min_Sea_Level_PressureIn, na.rm = TRUE),
    Min_Sea_Level_Press = mean(Min_Sea_Level_PressureIn, na.rm = TRUE), 
    Sd_Vis = sd(Min_VisibilityMiles, na.rm = TRUE),
    Min_Vis = mean(Min_VisibilityMiles, na.rm = TRUE), 
    Sd_CloudCover = sd(CloudCover, na.rm = TRUE),
    CloudCover = mean(CloudCover, na.rm = TRUE), 
    Sd_Mean_Wind_Speed = sd(Mean_Wind_SpeedMPH, na.rm = TRUE),
    Mean_Wind_Speed = mean(Mean_Wind_SpeedMPH, na.rm = TRUE), 
    Sd_Max_Wind_Speed = sd(Max_Wind_SpeedMPH, na.rm = TRUE),
    Max_Wind_Speed = mean(Max_Wind_SpeedMPH, na.rm = TRUE), 
    # Summarize error variables.
    mxT_sd = sd(maxTmp_diff, na.rm = TRUE), 
    mxT_mean = mean(maxTmp_diff, na.rm = TRUE), 
    mnT_sd = sd(minTmp_diff, na.rm = TRUE), 
    mnT_mean = mean(minTmp_diff, na.rm = TRUE), 
    mxT_sd_abs = sd(abs(maxTmp_diff), na.rm = TRUE), 
    mxT_mean_abs = mean(abs(maxTmp_diff), na.rm = TRUE), 
    mnT_sd_abs = sd(abs(minTmp_diff), na.rm = TRUE), 
    mnT_mean_abs = mean(abs(minTmp_diff), na.rm = TRUE), 
    BSN = sum(BrS, na.rm = TRUE)/sum(!is.na(BrS)),
    BSD = sum(BrSC, na.rm = TRUE)/sum(!is.na(BrSC))
  ) %>%
  mutate(BSS = 1 - BSN/BSD) %>%
  dplyr::select(-BSN, -BSD) %>%
  filter(!is.na(lag)) %>%
  left_join(., city, by = "AirPtCd")

combSum3$mxT_sd[is.nan(combSum3$mxT_sd)] <- NA
combSum3$mxT_mean[is.nan(combSum3$mxT_mean)] <- NA
combSum3$mnT_sd[is.nan(combSum3$mnT_sd)] <- NA
combSum3$mnT_mean[is.nan(combSum3$mnT_mean)] <- NA
combSum3$mxT_sd_abs[is.nan(combSum3$mxT_sd_abs)] <- NA
combSum3$mxT_mean_abs[is.nan(combSum3$mxT_mean_abs)] <- NA
combSum3$mnT_sd_abs[is.nan(combSum3$mnT_sd_abs)] <- NA
combSum3$mnT_mean_abs[is.nan(combSum3$mnT_mean_abs)] <- NA
#=============================================================================


# Step 3.4 - Aggregate results by airport
#=============================================================================
# Aggregate by airport 
combSum4 <- comb %>% group_by(AirPtCd) %>%
  summarise(
    Sd_Max_Temp = sd(Max_TemperatureF, na.rm = TRUE),
    Max_Temp = mean(Max_TemperatureF, na.rm = TRUE),
    Sd_Min_Temp = sd(Min_TemperatureF, na.rm = TRUE),
    Min_Temp = mean(Min_TemperatureF, na.rm = TRUE),
    Sd_Precip = sd(PrecipitationIn, na.rm = TRUE),
    Mean_Precip = mean(PrecipitationIn, na.rm = TRUE), 
    Sd_Max_DewPoint = sd(Max_Dew_PointF, na.rm = TRUE),
    Max_DewPoint = mean(Max_Dew_PointF, na.rm = TRUE), 
    Sd_Min_DewPoint = sd(Min_DewpointF, na.rm = TRUE),
    Min_DewPoint = mean(Min_DewpointF, na.rm = TRUE),
    Sd_Max_Humidity = sd(Max_Humidity, na.rm = TRUE),
    Max_Humidity = mean(Max_Humidity, na.rm = TRUE), 
    Sd_Min_Humidity = sd(Min_Humidity, na.rm = TRUE),
    Min_Humidity = mean(Min_Humidity, na.rm = TRUE),
    Sd_Max_Sea_Level_Press = sd(Max_Sea_Level_PressureIn, na.rm = TRUE),
    Max_Sea_Level_Press = mean(Max_Sea_Level_PressureIn, na.rm = TRUE), 
    Sd_Min_Sea_Level_Press = sd(Min_Sea_Level_PressureIn, na.rm = TRUE),
    Min_Sea_Level_Press = mean(Min_Sea_Level_PressureIn, na.rm = TRUE), 
    Sd_Vis = sd(Min_VisibilityMiles, na.rm = TRUE),
    Min_Vis = mean(Min_VisibilityMiles, na.rm = TRUE), 
    Sd_CloudCover = sd(CloudCover, na.rm = TRUE),
    CloudCover = mean(CloudCover, na.rm = TRUE), 
    Sd_Mean_Wind_Speed = sd(Mean_Wind_SpeedMPH, na.rm = TRUE),
    Mean_Wind_Speed = mean(Mean_Wind_SpeedMPH, na.rm = TRUE), 
    Sd_Max_Wind_Speed = sd(Max_Wind_SpeedMPH, na.rm = TRUE),
    Max_Wind_Speed = mean(Max_Wind_SpeedMPH, na.rm = TRUE), 
    # Summarize error variables.
    mxT_sd = sd(maxTmp_diff, na.rm = TRUE), 
    mxT_mean = mean(maxTmp_diff, na.rm = TRUE), 
    mnT_sd = sd(minTmp_diff, na.rm = TRUE), 
    mnT_mean = mean(minTmp_diff, na.rm = TRUE), 
    mxT_sd_abs = sd(abs(maxTmp_diff), na.rm = TRUE), 
    mxT_mean_abs = mean(abs(maxTmp_diff), na.rm = TRUE), 
    mnT_sd_abs = sd(abs(minTmp_diff), na.rm = TRUE), 
    mnT_mean_abs = mean(abs(minTmp_diff), na.rm = TRUE), 
    BSN = sum(BrS, na.rm = TRUE)/sum(!is.na(BrS)),
    BSD = sum(BrSC, na.rm = TRUE)/sum(!is.na(BrSC))
  ) %>%
  mutate(BSS = 1 - BSN/BSD) %>%
  dplyr::select(-BSN, -BSD) %>%
  left_join(., city, by = "AirPtCd")

combSum4$mxT_sd[is.nan(combSum4$mxT_sd)] <- NA
combSum4$mxT_mean[is.nan(combSum4$mxT_mean)] <- NA
combSum4$mnT_sd[is.nan(combSum4$mnT_sd)] <- NA
combSum4$mnT_mean[is.nan(combSum4$mnT_mean)] <- NA
combSum4$mxT_sd_abs[is.nan(combSum4$mxT_sd_abs)] <- NA
combSum4$mxT_mean_abs[is.nan(combSum4$mxT_mean_abs)] <- NA
combSum4$mnT_sd_abs[is.nan(combSum4$mnT_sd_abs)] <- NA
combSum4$mnT_mean_abs[is.nan(combSum4$mnT_mean_abs)] <- NA
#=============================================================================

# Step 4 - Record results. 
#=============================================================================
write.csv(combSum1, file = "../../data/summary_city_month_lag.csv", quote = TRUE,
          row.names = FALSE)

write.csv(combSum2, file = "../../data/summary_city_month.csv", quote = TRUE,
          row.names = FALSE)

write.csv(combSum3, file = "../../data/summary_city_lag.csv", quote = TRUE,
          row.names = FALSE)

write.csv(combSum4, file = "../../data/summary_city.csv", quote = TRUE,
          row.names = FALSE)

#=============================================================================


# Record the shiny app data file. This file combines the data frame summarized
# by lag as well as the overall aggregation, denoted as "lag 10".
combSum4$lag <- 10
combSum4 <- combSum4[, names(combSum3)]

combSum5 <- rbind.data.frame(combSum3, combSum4)
write.csv(combSum5, file = "../../data/summary_lag_shiny.csv", quote = TRUE,
          row.names = FALSE)

