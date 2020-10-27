# Final Weather Observations cleaning script
# Created: 6-21-2018
# Last Modified: 9-7-2018
# Description:
#   Script that consolidates all data cleaning steps taken throughout the 
#   Project. The output of the script is intended to be the final cleaned
#   version of the weather observations file. 
#=============================================================================

# Working directory should be set to the source file location prior to running.

# Load necessary packages. 
library(dplyr)
library(tidyr)
library(tibble)
library(lubridate)

# Step 1 - prepping original data file. 
#=============================================================================
weather <- read.csv("../../data/histWeather.csv")

weather$PrecipitationIn <- as.character(weather$PrecipitationIn)
summary(weather$PrecipitationIn)

# Assign an arbitrarily small amount of precipitation to "trace" days.
weather$PrecipitationIn[weather$PrecipitationIn == "T"] <- 0.005
weather$PrecipitationIn <- as.numeric(weather$PrecipitationIn)
summary(weather$PrecipitationIn)

weather$Date <- as.Date(as.character(weather$Date), format = "%m/%d/%Y")

# Convert to dplyr table format. 
weather <- as_tibble(weather)
#=============================================================================

# Step 2 - Add necessary data to replace systematic missing values. 
#=============================================================================
# Add the Denver precipication data
denver <- read.csv("../../data/DenverPrecip.csv")
# Change "Trace" values to arbitarily small amount of rain. 
trace <- regexpr(text = denver$PRCP_ATTRIBUTES, pattern = "^T") > 0
denver$PRCP[trace] <- 0.005
denver$DATE <- as.Date(denver$DATE, format = "%Y-%m-%d")
head(denver)

weather[weather$AirPtCd == "KBJC", ]$PrecipitationIn <- 
  denver$PRCP[match(weather[weather$AirPtCd == "KBJC", ]$Date, denver$DATE)]
# head(weather[weather$AirPtCd == "KBJC", ][, c(1, 20)], 10)
# tail(weather[weather$AirPtCd == "KBJC", ][, c(1, 20)], 10)

# Add the Baltimore wind data
baltWind <- read.csv("../../data/baltWind.csv")
baltWind$DATE <- as.Date(baltWind$DATE, format = "%Y-%m-%d")
weather[weather$AirPtCd == "KDMH", ]$Mean_Wind_SpeedMPH <- 
  round(baltWind$AWND[match(weather[weather$AirPtCd == "KDMH", ]$Date, baltWind$DATE)])
weather[weather$AirPtCd == "KDMH", ]$Max_Wind_SpeedMPH <- 
  round(baltWind$WSF2[match(weather[weather$AirPtCd == "KDMH", ]$Date, baltWind$DATE)])
weather[weather$AirPtCd == "KDMH", ]$Max_Gust_SpeedMPH <- 
  round(baltWind$WSF5[match(weather[weather$AirPtCd == "KDMH", ]$Date, baltWind$DATE)])
#=============================================================================

# Step 3 - Clean Data (consolidated from several files)
#=============================================================================
## Clean Temperature Variables
# Remove low temperatures in Hawaii
weather$Min_TemperatureF[weather$AirPtCd == "PHNL" & weather$Min_TemperatureF < 10] <- NA
weather$Mean_TemperatureF[weather$AirPtCd == "PHNL" & weather$Mean_TemperatureF < 50] <- NA

# Remove low mean temp measurements for all of Florida
FL.code <- c("KMIA", "KNIP", "KEYW", "KTPA")
weather$Mean_TemperatureF[is.element(weather$AirPtCd, FL.code) & weather$Mean_TemperatureF < 10] <- NA

# Remove anamalous low and mean temperatures in San Fransico (added 6-21-2018)
# This is consistent with record temperature recorded in San Fransisco.
# http://www.intellicast.com/local/history.aspx?location=USCA0987
weather$Min_TemperatureF[weather$AirPtCd == "KSFO" & weather$Min_TemperatureF < 20] <- NA
weather$Mean_TemperatureF[weather$AirPtCd == "KSFO" & weather$Min_TemperatureF < 20] <- NA

# Remove further temperature anomalies that we had intentionally left for the 
# homework in class. 
weather$Max_TemperatureF[weather$Max_TemperatureF > 200] <- NA
weather$Min_TemperatureF[weather$Min_TemperatureF < -100] <- NA
weather$Mean_TemperatureF[weather$Mean_TemperatureF < -100] <- NA

## Clean up anamalous precitation values. 

# There is a clear outlier preciptation reading in Oklahoma on 8-10-2017. 
# The precipitation for a nearby OKC station (KOKC Will Rogers Airport)
weather$PrecipitationIn[weather$AirPtCd == "KTIK" & weather$Date == "2017-08-10"] <- 0.8

# Clear outlier precipitation readings for Salmon Idaho. We will try to replace
# them eventually.
weather[weather$AirPtCd == "KSMN" & weather$PrecipitationIn > 2, ]
weather$PrecipitationIn[weather$AirPtCd == "KSMN" & weather$Date == "2015-04-21"] <- 0
weather$PrecipitationIn[weather$AirPtCd == "KSMN" & weather$Date == "2016-05-02"] <- 0
weather$PrecipitationIn[weather$AirPtCd == "KSMN" & weather$Date == "2017-05-03"] <- 0
weather$PrecipitationIn[weather$AirPtCd == "KSMN" & weather$Date == "2017-05-04"] <- 0

# Eliminate crazy day for Flagstaff, AZ
# Use the maximum precipitation of stations immediately 
# surrounding flagstaff on that day.
weather$PrecipitationIn[weather$AirPtCd == "KFLG" & weather$Date == "2016-12-25"] <- 0.97

# Eliminate crazy day for Indianapolis
# Replace with the recorded precipitation at the indianapolis airport 
# (which was nothing)
# SHOULD BE July 15, not 11. 
weather$PrecipitationIn[weather$AirPtCd == "KEYE" & weather$Date == "2015-07-15"] <- 0

# High days for Savannah, GA, and Charleston, SC seemed to be confirmed in the literature. 
# We will not remove these potential outlier values. 

## Clean up wind variables. 
# After spending several hours exploring wind speeds with the script "windSpeedCheck.R" 
# the following rules of thumb are used to guard against outliers. 

# Eliminate ALL wind observations when the mean wind speed exceeds 40 mph.
weather$Max_Wind_SpeedMPH[weather$Mean_Wind_SpeedMPH > 40] <- NA
weather$Mean_Wind_SpeedMPH[weather$Mean_Wind_SpeedMPH > 40] <- NA
weather$Max_Gust_SpeedMPH[weather$Mean_Wind_SpeedMPH > 40] <- NA

# Eliminate Max wind observations when the max wind speed exceeds 70 mph
weather$Max_Wind_SpeedMPH[weather$Max_Wind_SpeedMPH > 70] <- NA

# Eliminate max gust when the max gust exceeds 100 mph
weather$Max_Gust_SpeedMPH[weather$Max_Gust_SpeedMPH > 100] <- NA

# Eliminate max wind or max gust when they fall below the mean. 
weather$Max_Wind_SpeedMPH[weather$Max_Wind_SpeedMPH < weather$Mean_Wind_SpeedMPH] <- NA
weather$Max_Gust_SpeedMPH[weather$Max_Gust_SpeedMPH < weather$Mean_Wind_SpeedMPH] <- NA

# Impute max gust with max wind when it is missing
weather$Max_Gust_SpeedMPH[is.na(weather$Max_Gust_SpeedMPH)] <- 
  weather$Max_Wind_SpeedMPH[is.na(weather$Max_Gust_SpeedMPH)]

# Take the minimum value between max gust and max wind as the max wind value for that day.
# As a side note, max wind should NEVER exceed max gust anyway. 
for(i in 1:nrow(weather)){
  if(!is.na(weather$Max_Gust_SpeedMPH[i]) & !is.na(weather$Max_Wind_SpeedMPH[i])){
    if(weather$Max_Gust_SpeedMPH[i] < weather$Max_Wind_SpeedMPH[i]){
      weather$Max_Wind_SpeedMPH[i] <- weather$Max_Gust_SpeedMPH[i]
    }
  }
}

## Clean up visibility

# 10 is a commonly accepted number for max visibility within the dataset
# restrict all values to be no larger than this (and no smaller than 0).
# (added 6-21-2018)
# Note that the Max_VisibilityMile variable is almost always 10 and probably
# not useful in the final cluster analysis. 
weather$Max_VisibilityMiles[weather$Max_VisibilityMiles > 10] <- 10
weather$Min_VisibilityMiles[weather$Min_VisibilityMiles > 10] <- 10
weather$Mean_VisibilityMiles[weather$Mean_VisibilityMiles > 10] <- 10
weather$Max_VisibilityMiles[weather$Max_VisibilityMiles < 0] <- NA
weather$Min_VisibilityMiles[weather$Min_VisibilityMiles < 0] <- NA
weather$Mean_VisibilityMiles[weather$Mean_VisibilityMiles < 0] <- NA

## Clean up dew point (based on obvious outliers in diagnostic box plots)
weather$Min_DewpointF[weather$AirPtCd == "PHNL" & weather$Min_DewpointF < 40] <- NA
weather$Min_DewpointF[weather$AirPtCd == "KHQM" & weather$Min_DewpointF < 0] <- NA
weather$MeanDew_PointF[weather$AirPtCd == "KHQM" & weather$MeanDew_PointF < 10] <- NA
weather$Min_DewpointF[weather$AirPtCd == "KVGT" & weather$Min_DewpointF < -15] <- NA
weather$Min_DewpointF[weather$AirPtCd == "KBJC" & weather$Min_DewpointF < -20] <- NA

## Clean up humidity
# Humidity can never be 0...happens 50 times for max and mean 
# and 55 times for min
weather$Max_Humidity[weather$Max_Humidity == 0] <- NA
weather$Mean_Humidity[weather$Mean_Humidity == 0] <- NA

# Whenever max, mean, AND min humidity are all 0, set min equal to NA
sumHumid <- weather$Max_Humidity + weather$Mean_Humidity + weather$Min_Humidity
weather$Min_Humidity[sumHumid == 0] <- NA

## Clean up pressure
# Not enough variability to determine what is actually an outlier. Plus there is no 
# way have any sense of what would be appropriate. I am going to leave this one alone. 

## Clean up other variables. 

weather$Max_Humidity[weather$Max_Humidity > 100] <- NA
weather$Min_Humidity[weather$Min_Humidity > 100] <- NA
weather$Mean_Humidity[weather$Mean_Humidity > 100] <- NA
weather$CloudCover[weather$CloudCover < 0] <- NA
weather$WindDirDegrees[weather$WindDirDegrees < 0] <- NA
weather$Min_DewpointF[weather$Min_DewpointF < -50] <- NA
#=============================================================================

# RECORD THE FINAL CLEANED DATASET
write.csv(weather, "../../data/final_weather.csv", quote = FALSE, row.names = FALSE)






