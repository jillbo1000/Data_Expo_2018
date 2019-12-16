setwd("C:/Users/jflun/Dropbox/USU/Advanced Graphics II/Data Expo/DataExpo2018")

library(ggmap)
library(tidyverse)
library(RColorBrewer)
library(grid)
library(gtable)
library(reshape2)
# library(cowplot) # don't load explicitly as it changes default ggplot themes

# Load weather and forecast data
weather.final <- read.csv("Data/final_weather.csv")
forecasts <- read.csv("Data/final_forecast.csv")
locations <- read.csv("Data/locationsFinal.csv")

# Identify forecasts by Airport code
forecasts$AirPtCd <- locations$AirPtCd[forecasts$city]

# Rename Date variable in forecast to match weather.final
forecasts$Date <- forecasts$fdate

# Remove city variable. 
forecasts$city <- NULL

# Combine weather and forecast data
# Filter out irrelevant results
# Eliminate duplicates
# Take the difference between predicted temp and actual temp for daily 
# maximum and minimum temperatures. 
comb = full_join(forecasts, weather.final)
head(comb)

comb$maxTmp_diff = comb$MaxTemp - comb$Max_TemperatureF
comb$minTmp_diff = comb$MinTemp - comb$Min_TemperatureF
comb$Date = as.Date(comb$Date, format = "%Y-%m-%d")
comb$date = as.Date(comb$date, format = "%Y-%m-%d")
head(comb)
summary(comb)

# Limit the dataset to day of predictions
comb1 = comb[(comb$lag == 0 & !is.na(comb$lag)), ]


# Lets try and make some grobs and overlay them on a ggmap
# Code adapted from:
# - https://stackoverflow.com/questions/36063043/how-to-plot-barchart-onto-ggplot2-map
# Remove tick marks syntax:
# - http://www.sthda.com/english/wiki/ggplot2-axis-ticks-a-guide-to-customize-tick-marks-and-labels#hide-x-and-y-axis-tick-mark-labels 
#tcol <- brewer.pal(9, "Blues")
tcol <- brewer.pal(5, "BrBG")
tcol <- tcol[c(1, 5)]

# Control size of glyphs (distance to center)
rad <- 1.4

# -2 will prevent plots for Alaska and Hawaii (just want a working example, 
# can add later if we want to go this direction.)
n = nrow(locations)
# Create grobs of each glyph
p1.grobs = vector("list", n)

# Create a list of grobs. Each grob is a time series graph of the 
# temperature difference (predicted - actual) for every prediction. 
# All axes and other panel properties have been stripped so
# that ONLY the lines are plotted. The limits on the y-axis have 
# been controlled so that the scale of each time series plot is identical.
# A few outliers were found so the scale was reduced to remove the most 
# extreme ones.
for(i in 1:n){
  
  p1 <- ggplot(data = comb1[comb1$AirPtCd == locations$AirPtCd[i], ], 
               aes(x = date, y = maxTmp_diff)) + 
    geom_line() +
    ylim(-35, 35) + 
    theme(legend.position = "none", 
          rect = element_blank(), text = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.ticks = element_blank() ) 
  
  p1.grobs[[i]] <- ggplotGrob(p1)
  
  # Not exactly sure what this does, but I believe it makes sure that the plot
  # fills the entire space of the panel. 
  #panel_coords <- p1.grobs[[i]]$layout[p1.grobs[[i]]$layout$name == "panel",]
  #p1.grobs[[i]][panel_coords$t:panel_coords$b, panel_coords$l:panel_coords$r]
}
# The inset function allows us to place a subplot on a ggmap, we use the inset
# function to plot our grobs.

glyph.test <- lapply(1:(n-2), function(i) 
  inset(p1.grobs[[i]], 
        xmin = locations$longitude[locations$AirPtCd == 
                                     locations$AirPtCd[i]] - rad,
        xmax = locations$longitude[locations$AirPtCd == 
                                     locations$AirPtCd[i]] + rad,
        ymin = locations$latitude[locations$AirPtCd == 
                                    locations$AirPtCd[i]] - rad,
        ymax = locations$latitude[locations$AirPtCd == 
                                    locations$AirPtCd[i]] + rad ) )


# Now create map of interest
# Only uncomment when needing to re-query for the map. Query seems quite unstable.
map.48 <- get_googlemap(zoom = 3, 
                        maptype = "terrain", color= "bw",
                        style = 'feature:all|element:labels|visibility:off')

map.test <- ggmap(map.48, extent = "device") + 
  xlim(min(locations[1:(n-2), ]$longitude) - 1, 
       max(locations[1:(n-2), ]$longitude) + 2) + 
  ylim(min(locations[1:(n-2), ]$latitude) - 1, 
       max(locations[1:(n-2), ]$latitude) + 2)

# Now lets add Hawaii and Alaska maps to the mix.
map.AK <- get_googlemap(center = c(lon = -150, lat = 65), zoom = 4, 
                        maptype = "terrain", color= "bw",
                        style = 'feature:all|element:labels|visibility:off')
AK.map <- ggmap(map.AK, extent = "device") + 
  xlim(-168, -142) + ylim(55, 71)

#AK.map 

AK.grob <- ggplotGrob(AK.map)

map.HW <- get_googlemap(center = c(lon = -157.5, lat = 20.5), zoom = 7, 
                        maptype = "terrain", color= "bw",
                        style = 'feature:all|element:labels|visibility:off')

HW.map <- ggmap(map.HW, extent = "device") + 
  xlim(-160, -154.5) + ylim(18.75, 22.25)

#HW.map

HW.grob <- ggplotGrob(HW.map)

map.test2 <- map.test +   
  inset(AK.grob, xmin = -124, xmax = -117,
        ymin = 22, ymax = 32) +
  inset(HW.grob, xmin = -116.5, xmax = -104,
        ymin = 24, ymax = 30)

#map.test2

# Add glyphs for the lower 48.
result_plot <- Reduce(`+`, glyph.test, map.test2)

pdf("TimeSeriesGlyph1a.pdf", paper = "USr", width = 10, height = 8)
result_plot
dev.off()

# Add glyphs for AK (n-1) and HW (n) manually
result_plot2 <- result_plot +
  inset(p1.grobs[[n-1]], xmin = -119.5 - rad, xmax = -119.5 + rad,
        ymin = 25.5 - rad, ymax = 25.5 + rad) + 
  inset(p1.grobs[[n]], xmin = -111.5 - rad, xmax = -111.5 + rad,
        ymin = 28.2 - rad, ymax = 28.2 + rad)

pdf("TimeSeriesGlyph1b.pdf", paper = "USr", width = 10, height = 8)
result_plot2
dev.off()

png("TimeSeriesGlyph1b.png", width = 900, height = 750)
result_plot2
dev.off()









