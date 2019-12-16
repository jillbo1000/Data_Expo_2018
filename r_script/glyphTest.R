# install_github("garrettgman/ggsubplot")

library(ggmap)
library(tidyverse)
library(RColorBrewer)
library(grid)
library(gtable)

locations <- read.csv("../Data/locationsFinal.csv")

locations$longitude = round(locations$longitude)
locations$latitude = round(locations$latitude)

locations$latitude[locations$city == "Providence"] <- 41

# Filter idea from:
# - https://stackoverflow.com/questions/28244123/find-duplicated-elements-with-dplyr 
# Remove tick marks:
# - http://www.sthda.com/english/wiki/ggplot2-axis-ticks-a-guide-to-customize-tick-marks-and-labels#hide-x-and-y-axis-tick-mark-labels 
checkIt <- locations %>% 
  dplyr::select(latitude, longitude, AirPtCd, city, state) %>% 
  group_by(latitude, longitude) %>% 
  filter(n() > 1)

checkit2 <- locations %>% filter(is.element(city, c("Boston", "Providence")))

weather.final <- read.csv("../Data/final_weather.csv")

#weather.sub <- weather.final %>% filter(city == "Boston")

forecasts <- read.csv("../Data/final_forecast.csv")

forecasts$AirPtCd <- locations$AirPtCd[forecasts$city]

forecasts$Date <- forecasts$fdate

forecasts$city <- NULL

forecasts2 <- forecasts %>% 
  mutate(Date = gsub(as.character(Date), pattern = "-", replacement = ""),
         Date = as.numeric(Date),
         year = floor(Date/10000), 
         month = floor((Date %% 10000)/100))

weather.combined <- weather.final %>% 
  select(AirPtCd, Date, Max_TemperatureF) %>% 
  left_join(., forecasts, by = c("AirPtCd", "Date")) %>%
  mutate(Date = gsub(as.character(Date), pattern = "-", replacement = ""),
         Date = as.numeric(Date), 
         year = floor(Date/10000), 
         month = floor((Date %% 10000)/100)) %>% 
  filter(Date < 20170901, lag < 7) %>%
  dplyr::select(AirPtCd, year, month, lag, Max_TemperatureF, MaxTemp, Date) %>%
  na.omit() %>%
  group_by(AirPtCd, Date, lag) %>% slice(1) %>% ungroup() %>%
  group_by(AirPtCd, month, lag) %>%
  summarize(mMax = mean(abs(Max_TemperatureF - MaxTemp)),
            n = n())

weather.combined <- weather.combined %>% na.omit() %>%
  mutate(tMonth = month + 12*(year - 2014)) %>% filter(n > 10)

#plot(weather.combined$mMax[weather.combined$AirPtCd == "KAVP"], type = "l")


weather.combined$dRad <- ((2*pi)/11) * (weather.combined$month - 1)


tcol <- brewer.pal(8, "PuBu")

n = nrow(locations) - 2 
# Create grobs of each glyph
p1.grobs = vector("list", n)
for(i in 1:n){
  
p1 <- ggplot(data = weather.combined[weather.combined$AirPtCd == 
                                       locations$AirPtCd[i], ], 
             aes(x = dRad, y = mMax)) + 
  geom_line(aes(col = as.factor(lag))) +
  scale_color_manual(values = tcol[-1]) +
  ylim(0, 10) + 
  geom_point(aes(x = 0, y = 0), size = I(0.5)) + 
  coord_polar(theta = "x") + 
  theme(legend.position = "none", 
        rect = element_blank(), text = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank() ) 

p1.grobs[[i]] <- ggplotGrob(p1)

panel_coords <- p1.grobs[[i]]$layout[p1.grobs[[i]]$layout$name == "panel",]
p1.grobs[[i]][panel_coords$t:panel_coords$b, panel_coords$l:panel_coords$r]
}


glyph.test <- lapply(1:n, function(i) 
  inset(p1.grobs[[i]], 
                    xmin = round(locations$longitude[locations$AirPtCd == locations$AirPtCd[i]]) - 1.5,
                    xmax = round(locations$longitude[locations$AirPtCd == locations$AirPtCd[i]]) + 1.5,
                    ymin = round(locations$latitude[locations$AirPtCd == locations$AirPtCd[i]]) - 1.5,
                    ymax = round(locations$latitude[locations$AirPtCd == locations$AirPtCd[i]]) + 1.5 ) )


# Now create map of interest
# Only uncomment when needing to re-query for the map. Query seems quite unstable.
#map.48 <- get_map(location = "United States", zoom = 3, 
#                  maptype = "terrain", color= "bw")

map.test <- ggmap(map.48, extent = "device") + 
  xlim(min(loc.temps$longitude) - 2, max(loc.temps$longitude) + 2) + 
  ylim(min(loc.temps$latitude) - 2, max(loc.temps$latitude) + 2)

result_plot <- Reduce(`+`, glyph.test, map.test)


result_plot

 

# Lets try and make some grobs and overlay
# Code adapted from:
# - https://stackoverflow.com/questions/36063043/how-to-plot-barchart-onto-ggplot2-map
set.seed(1)
geo_data <- data.frame(who=rep(c(1:length(map.kt$OBJECTID)), each=2),
                       value=as.numeric(sample(1:100, length(map.kt$OBJECTID)*2, replace=T)),
                       id=rep(c(1:length(map.kt$OBJECTID)), 2))



