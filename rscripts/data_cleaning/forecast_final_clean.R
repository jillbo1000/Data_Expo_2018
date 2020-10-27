# Final Weather Forecast cleaning script
# Created: 6-21-2018
# Last Modified: 6-21-2018
# Description:
#   Script that consolidates all data cleaning steps taken throughout the 
#   Project. The output of the script is intended to be the final cleaned
#   version of the weather forecast file. 
#=============================================================================

# Set working directory to source file location prior to running. 

library(dplyr)
library(lubridate)
library(tidyr)

forecast <- read.table("../../data/forecast.dat")
head(forecast)
colnames(forecast) <- c("city", "fdate", "value", "type", "date")
summary(forecast)

forecast$fdate <- as.Date(as.character(forecast$fdate), format = "%Y-%m-%d")
forecast$date <- as.Date(as.character(forecast$date), format = "%Y-%m-%d")

tmp <- as.numeric(as.character(forecast$value))
# summary(tmp)

# forecast[is.na(tmp), ]
# forecast$value[is.na(tmp)]

forecast$value <- as.numeric(as.character(forecast$value))
# summary(forecast)

# Read in city data
city <- read.csv("../../data/locations.csv")
head(forecast)

# Make plots for outlier identification - no outliers or other
# data anomalies identified
type <- c("MaxTemp", "MinTemp", "ProbPrecip")
pos <- c(1, 10, 20, 30)

# pdf("DataExpo2018/../data Cleaning/Forecast_Outlier_Plots.pdf", height = 9, width = 6.5)
# par(mfrow = c(4, 3), oma = c(0, 0, 2, 0))
# for(i in 1:113) {
#   tmp <- forecast[forecast$city == i, ]
#   
#   for(j in 1:3) {
#     tmp2 <- tmp[tmp$type == type[j], ]
#     qqnorm(tmp2$value, main = paste(type[j], ", ", city$city[i], ", ", city$state[i]))
#     qqline(tmp2$value)
#   }
#   
# }
# dev.off()

# Distinguish between the morning and 
head(forecast[forecast$type == "ProbPrecip", ])
precip <- forecast[forecast$type == "ProbPrecip", ]

precip$time <- 1
head(precip)

for(i in 1:113) {
  tmp <- precip[precip$city == i, ]
  tmp$time[duplicated(cbind(tmp$fdate, tmp$date))] <- 2
  precip$time[precip$city == i] <- tmp$time
}

head(precip, 20)
tail(precip, 20)

forecast$time <- NA
forecast$time[forecast$type == "ProbPrecip"] <- precip$time
head(forecast)
head(forecast[forecast$type == "ProbPrecip", ])

# Move data around to be tidy 

# Change type of PrecipProb to PrecipAM and PrecipPM
tmp <- ifelse(forecast$time[forecast$type == "ProbPrecip"] == 1, "PrecipAM", "PrecipPM")

forecast$type <- as.character(forecast$type)
forecast$type[forecast$type == "ProbPrecip"] <- ifelse(forecast$time[forecast$type == "ProbPrecip"] == 1, "PrecipAM", "PrecipPM")
table(forecast$type)

# Separate data into four datasets
maxTmp <- forecast[forecast$type == "MaxTemp", c(1, 2, 5, 3)]
colnames(maxTmp)[4] <- "MaxTemp"
minTmp <- forecast[forecast$type == "MinTemp", c(1, 2, 5, 3)]
colnames(minTmp)[4] <- "MinTemp"
precipAM <- forecast[forecast$type == "PrecipAM", c(1, 2, 5, 3)]
colnames(precipAM)[4] <- "PrecipAM"
precipPM <- forecast[forecast$type == "PrecipPM", c(1, 2, 5, 3)]
colnames(precipPM)[4] <- "PrecipPM"

# Remove NAs from datasets and look for duplicates
head(maxTmp)
mxt.dup <- maxTmp[duplicated(maxTmp[, 1:3], fromLast = TRUE), ]
dim(mxt.dup)
mxt.dup

mxt.na <- maxTmp[is.na(maxTmp$MaxTemp), ]
dim(mxt.na)
head(mxt.na)

maxTmp <- maxTmp[!is.na(maxTmp$MaxTemp), ]
head(maxTmp)
dim(maxTmp)

# There are a ton of duplicate min temps. One is systematically
# much larger than the other. It appears that one is a daily low
# and one is a nightly low. I decided to keep the lower of the two
# because that is what the min temp on a forecast typically
# represents.
mnt.dup1 <- duplicated(minTmp[, 1:3], fromLast = TRUE)
mnt.dup2 <- duplicated(minTmp[, 1:3])
mnt.dup <- minTmp[(mnt.dup1 | mnt.dup2), ]
dim(mnt.dup)
head(mnt.dup)
mnt.dup[mnt.dup1 * mnt.dup2, ]
forecast[1509636:1509642, ]
mnt.dup[(mnt.dup$city == 10 & mnt.dup$date == "2016-01-14"), ]

mnt.na <- minTmp[is.na(minTmp$MinTemp), ]
dim(mnt.na)
head(mnt.na)

minTmp <- minTmp[!is.na(minTmp$MinTemp), ]
head(minTmp)
dim(minTmp)

pam.dup <- precipAM[duplicated(precipAM[, 1:3]), ]
dim(pam.dup)
pam.dup

pam.na <- precipAM[is.na(precipAM$PrecipAM), ]
dim(pam.na)
head(pam.na)

ppm.dup1 <- duplicated(precipPM[, 1:3])
ppm.dup2 <- duplicated(precipPM[, 1:3], fromLast = TRUE)
ppm.dup <- precipPM[(ppm.dup1 | ppm.dup2), ]
dim(ppm.dup)
head(ppm.dup, 20)
summary(ppm.dup)
sum(ppm.dup$PrecipPM > 0)

ppm.na <- precipPM[is.na(precipPM$PrecipPM), ]
dim(ppm.na)
head(ppm.na)

# Remove duplicates from minTmp. Only the lowest minTemp was kept from the pairs. 
minTmp <- minTmp[order(minTmp$MinTemp), ]
minTmp <- minTmp[!duplicated(minTmp[, 1:3]), ]
head(minTmp)
dim(minTmp)

# precipPM also had a lot of duplicates. There was no discernable pattern 
# in the duplicates so I opted to keep the last one that was recorded. 
precipPM <- precipPM[!duplicated(precipPM[, 1:3], fromLast = TRUE), ]
dim(precipPM)
head(precipPM)

# The following code joins the four data measurements together into 
# a wide format dataset. 
tmp <- full_join(maxTmp, minTmp, by = c("city", "fdate", "date"))
head(tmp)

tmp2 <- full_join(tmp, precipAM, by = c("city", "fdate", "date"))
head(tmp2)

tmp3 <- full_join(tmp2, precipPM, by = c("city", "fdate", "date"))
head(tmp3, 10)
tail(tmp3, 20)

# Look for more duplicates in the dateset. 
# There were still 7 duplicates in the datesetThey all had an NA on the 
# minTemp for the second or third reading so I removed all but the first
# duplicated observation. The resulting dataset has no more duplicate dates.
dup.test1 <- duplicated(tmp3[, 1:3], fromLast = TRUE)
dup.test2 <- duplicated(tmp3[, 1:3], fromLast = FALSE)
dup.test <- tmp3[(dup.test1 | dup.test2), ]

tmp3 <- tmp3[!duplicated(tmp3[, 1:3]), ]
dim(tmp3)
sum(duplicated(tmp3[, 1:3]))

# Compute lag between the forecast and the date being forecasted.
# Several of the lags were -1 days, which means that the forecast was
# for the day before. 183 of the observations were in this category 
# so they were removed from the dataset.
tmp3$lag <- as.numeric(tmp3$fdate - tmp3$date)
summary(tmp3)

head(tmp3[tmp3$lag < 0, ], 10)
nrow(tmp3[tmp3$lag < 0, ]) # 183 forecasts from the day after

tmp3 <- tmp3[tmp3$lag > -1, ]
head(tmp3)

# One last data check before exporting the dataset. The data have
# a lot of NAs.
sum(duplicated(tmp3[, 1:3]))
summary(tmp3)

# Filter out day 7 lags as there are so few of them. 
# Filter out day 6 lags as many stations don't have a lag 6 forecast for
# minimum temperature. Those readings would probably be faulty anyway. 
tmp3 <- tmp3 %>% filter(lag < 6)

write.csv(tmp3, "../../data/final_forecast.csv", 
          quote = FALSE, row.names = FALSE)






