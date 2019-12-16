# Clean the Richfield, UT, Allen, NV, and Eastport, ME data and compare results
# to the expo data. 
library(tidyverse)

# First, read and format the NCDC weather data for these locations. 
weather.select <- read.csv("../Data/richfield_allen_data.csv")
weather.select2 <- read.csv("../Data/eastportData.csv")
weather.select3 <- read.csv("../Data/Denver Precipitation Data.csv")

weather.select <- bind_rows(weather.select, weather.select2)

# Read Original Measurements
measurements <- read.csv("../Data/final_weather.csv")

weather.new <- weather.select %>% 
  dplyr::select(-contains("WT")) %>%
  separate(col = PRCP_ATTRIBUTES, 
           into = c("Measurement.Flag", "Quality.Flag", 
                    "Source.Flag", "Time.of.Observation"), 
           fill = "right",
           sep = ",") %>% 
  separate(col = TMIN_ATTRIBUTES,
           into = c("Measurement.Flag.1", "Quality.Flag.1", 
                    "Source.Flag.1"),
           fill = "right",
           sep = ",") %>%
  separate(col = TMAX_ATTRIBUTES,
           into = c("Measurement.Flag.2", "Quality.Flag.2", 
                    "Source.Flag.2"),
           fill = "right",
           sep = ",") %>%
dplyr::select(STATION, NAME, ELEVATION, LATITUDE, LONGITUDE, DATE, PRCP, 
              Measurement.Flag, Quality.Flag, Source.Flag, Time.of.Observation, 
              TMIN, Measurement.Flag.1, Quality.Flag.1, Source.Flag.1,
              TMAX, Measurement.Flag.2, Quality.Flag.2, Source.Flag.2) %>% 
  # mutate(DATE = gsub(x = DATE, pattern = "-", replacement = "")) %>%
  replace_na(list(Measurement.Flag = "",
                  Quality.Flag = "",
                  Source.Flag = "", 
                  Time.of.Observation = "9999",
                  Measurement.Flag.1 = "",
                  Quality.Flag.1 = "",
                  Source.Flag.1 = "",
                  Measurement.Flag.2 = "",
                  Quality.Flag.2 = "",
                  Source.Flag.2 = "")) 

unique(weather.new$Quality.Flag) # No problems with PRCP
unique(weather.new$Quality.Flag.1) # A few problems with temperatures
unique(weather.new$Quality.Flag.2)

sum(weather.new$Quality.Flag.1 != "") # 5 problem obersvations with min temps
sum(weather.new$Quality.Flag.2 != "") # 5 problem observations with max temps

# Check these observations out
checkIt <- weather.new %>% filter(Quality.Flag.1 != "" | Quality.Flag.2 != "")

# No issues with Measurement Flags To Address
unique(weather.new$Measurement.Flag) 
unique(weather.new$Measurement.Flag.1) 
unique(weather.new$Measurement.Flag.2)
unique(weather.new$Measurement.Flag.3) 
unique(weather.new$Measurement.Flag.4)

# When quality issue arises, switch the given measurement to "missing"
weather.new$TMIN[weather.new$Quality.Flag.1 != ""] <- NA
weather.new$TMAX[weather.new$Quality.Flag.2 != ""] <- NA


# After applying NCDC quality assurance measures, check to see if there 
# are any obvious outliers in the temperature data. 
weather.temp <- weather.new %>% dplyr::select(NAME, TMIN) %>% na.omit()

st.names <- unique(weather.temp$NAME)

par(mfrow = c(2,2))
for(i in st.names){
  boxplot(weather.temp$TMIN[weather.temp$NAME == i], main = i)
}


weather.temp2 <- weather.new %>% dplyr::select(NAME, TMAX) %>% na.omit()

st.names <- unique(weather.temp2$NAME)

par(mfrow = c(2,2))
for(i in st.names){
  boxplot(weather.temp2$TMAX[weather.temp$NAME == i], main = i)
}

# No observable differences. 

# Now I need to pick which Austin Nevada station I will use. 
# See which station has more observations

weather.new %>% group_by(NAME) %>% tally()

checkit <- weather.new %>% group_by(NAME) %>% slice(1)
  
# Austin #2 is closer to the prediction site, with a more similar elevation. 

# Filter the three stations we will use to replace measurementes 
# in the original file. 

weather.final <- weather.new %>% 
  filter(is.element(NAME, c("RICHFIELD RADIO KSVC, UT US",
                            "AUSTIN NUMBER 2, NV US",
                            "ROBBINSTON, ME US"))) %>%
  mutate(city = gsub(NAME, pattern = ",?[[:space:]].*", 
                     replacement = ""),
         state = gsub(NAME, pattern = "[^,]*,[[:space:]]+", 
                      replacement = ""),
         state = gsub(state, pattern = "[[:space:]]+US$", 
                      replacement = ""),
         longitude = as.numeric(as.character(LONGITUDE)),
         latitude = as.numeric(as.character(LATITUDE)),
         AirPtCd = case_when(
           state == "NV" ~ "KP68",
           state == "UT" ~ "KMLF",
           state == "ME" ~ "KBHB"
         ),
         state = case_when(
           state == "NV" ~ "Nevada",
           state == "UT" ~ "Utah",
           state == "ME" ~ "Maine"
         ),
         Date = DATE,
         Max_TemperatureF = as.numeric(TMAX),
         Min_TemperatureF = as.numeric(TMIN),
         PrecipitationIn = PRCP) %>%
  dplyr::select(city, state, longitude, latitude, AirPtCd, 
                Date, Max_TemperatureF,
                Min_TemperatureF, PrecipitationIn)


# Now, lets make a new dataframe using these measurements 
# instead of the old ones.

readings <- read.csv("../Data/weather2.csv")

# Filter out airport readings we don't want. 
readings <- readings %>% 
  filter(!is.element(AirPtCd, c("KP68", "KMLF", "KBHB")))

readings2 <- data.table::rbindlist(list(readings, weather.final), fill = TRUE)

write.csv(weather.final, file = "../Data/newWeather.csv", row.names = FALSE)

  
  

