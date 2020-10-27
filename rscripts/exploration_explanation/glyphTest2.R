# install_github("garrettgman/ggsubplot")

library(ggmap)
library(tidyverse)
library(RColorBrewer)
library(grid)
library(gtable)
library(reshape2)
# library(cowplot) # don't load explicitly as it changes default ggplot themes

# Load weather and forecast data
weather.final <- read.csv("../../data/final_weather.csv")
forecasts <- read.csv("../../data/final_forecast.csv")
locations <- read.csv("../../data/locationsFinal.csv")

# Identify forecasts by Airport code
forecasts$AirPtCd <- locations$AirPtCd[forecasts$city]

# Rename Date variable in forecast to match weather.final
forecasts$Date <- forecasts$fdate

# Remove city variable. 
forecasts$city <- NULL

# Combine weather and forecast data
# Filter out irrelevant results
# Eliminate duplicates
# Take the mean difference for daily maximum and minimum temperatures. 
weather.combined <- weather.final %>% 
  select(AirPtCd, Date, Max_TemperatureF, Min_TemperatureF) %>% 
  left_join(., forecasts, by = c("AirPtCd", "Date")) %>%
  mutate(Date = gsub(as.character(Date), pattern = "-", replacement = ""),
         Date = as.numeric(Date), 
         year = floor(Date/10000), 
         month = floor((Date %% 10000)/100)) %>% 
  filter(Date < 20170901, lag < 7) %>%
  dplyr::select(AirPtCd, year, month, lag, Max_TemperatureF, MaxTemp, 
                Min_TemperatureF, MinTemp, Date) %>%
  group_by(AirPtCd, Date, lag) %>% slice(1) %>% ungroup() %>%
  group_by(AirPtCd, month) %>%
  summarize(mMax = mean(abs(Max_TemperatureF - MaxTemp), na.rm = TRUE),
            mMin = mean(abs(Min_TemperatureF - MinTemp), na.rm = TRUE),
            n = n())

# Create polar coordinate x-axis based on months. 
weather.combined$dRad <- ((2*pi)/11) * (weather.combined$month - 1)

# Melt the max and min temperatures into a common data frame. 
weather.combined <- melt(weather.combined, measure.vars = c("mMax", "mMin"))


# Lets try and make some grobs and overlay them on a ggmap
# Code adapted from:
# - https://stackoverflow.com/questions/36063043/how-to-plot-barchart-onto-ggplot2-map
# Remove tick marks syntax:
# - http://www.sthda.com/english/wiki/ggplot2-axis-ticks-a-guide-to-customize-tick-marks-and-labels#hide-x-and-y-axis-tick-mark-labels 
#tcol <- brewer.pal(9, "Blues")
tcol <- brewer.pal(5, "BrBG")
tcol <- tcol[c(1, 5)]

# Control size of glyphs (distance to center)
rad <- 2.2

# -2 will prevent plots for Alaska and Hawaii (just want a working example, 
# can add later if we want to go this direction.)
n = nrow(locations)
# Create grobs of each glyph
p1.grobs = vector("list", n)

# Create a list of grobs. Each grob is a cirucular glyph of the mean absolute
# temperature difference (predicted - actual) as averaged across months for 
# varying seasons. All axes and other panel properties have been stripped so
# that ONLY the lines are plotted. The dot in the center of each plot serves
# as a reference point for each plot, with circles symmetric about the dot
# having similar prediction errors throughout the year. 
for(i in 1:n){
  
  p1 <- ggplot(data = weather.combined[weather.combined$AirPtCd == 
                                         locations$AirPtCd[i], ], 
               aes(x = dRad, y = value)) + 
    geom_line(aes(col = as.factor(variable))) +
    scale_color_manual(values = tcol) +
    ylim(0, 12) + 
    geom_point(aes(x = 0, y = 0), pch = I(20),
               size = I(0.1), color = I("red")) + 
    coord_polar(theta = "x") + 
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

# Add glyphs for AK (n-1) and HW (n) manually
result_plot2 <- result_plot +
  inset(p1.grobs[[n-1]], xmin = -119.5 - rad, xmax = -119.5 + rad,
        ymin = 25.5 - rad, ymax = 25.5 + rad) + 
  inset(p1.grobs[[n]], xmin = -111.5 - rad, xmax = -111.5 + rad,
        ymin = 28.2 - rad, ymax = 28.2 + rad)


# Create a "compass" plot for glyph reference. 
p1 <- ggplot2::ggplot(data = weather.combined[weather.combined$AirPtCd == 
                                                locations$AirPtCd[1], ], 
                      aes(x = dRad, y = value)) + 
  #geom_line(aes(col = as.factor(lag))) +
  #geom_polygon(aes(col = as.factor(lag)), fill = NA) +
  geom_blank() + 
  scale_color_manual(name = "Lag", values = tcol) +
  scale_x_continuous(name = "Months",
                     breaks = ((2*pi)/11) * 0:11) + 
  scale_y_continuous(name = "Absolute Difference",
                     breaks = seq(0, 4, 4)) +
  geom_point(aes(x = 0, y = 0), pch = I(20),
             size = I(0.1), color = I("red")) +
  geom_text(data = data.frame(y = c(7, rep(6, 10)), x = ((2*pi)/11) * 0:10), 
            aes(x = x, y = y,
            label = c("Jan/Dec", "Feb", "Mar", "Apr", "May",
                     "Jun", "Jul", "Aug", "Sep", "Oct",
                     "Nov"))) + 
  coord_polar(theta = "x") +
  guides(colour = guide_legend(override.aes = list(lwd=3))) + 
  theme(
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(size = 10),
        axis.title.y =  element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        plot.margin = margin(0, 0, 0, 0, "pt"),
        rect = element_blank())

# Create a plot to extract ONLY the legend
p1.2 <- ggplot(data = weather.combined[weather.combined$AirPtCd == 
                                         locations$AirPtCd[1], ], 
               aes(x = dRad, y = value)) + 
  geom_line(aes(col = as.factor(variable))) +
  scale_color_manual(name = "Measurement", values = tcol,
                     labels = c("Maximum Temperature",
                                "Minimum Temperature")) +
  ylim(0, 10) + 
  geom_point(aes(x = 0, y = 0), pch = I(20),
             size = I(0.1), color = I("red")) + 
  coord_polar(theta = "x") + 
  theme(legend.position = "top",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        rect = element_blank()) +
  guides(color=guide_legend(nrow=1, override.aes = list(lwd=3)))

# Function from:
# - https://gist.github.com/crsh/be88be19233f1df4542aca900501f0fb 
gglegend <- function(x){ 
  tmp <- ggplot_gtable(ggplot_build(x)) 
  leg <- which(sapply(tmp$grobs, function(y) y$name) == "guide-box") 
  tmp$grobs[[leg]]
}

# Extract only the legend of a ggplot. 
legend <- gglegend(p1.2) 

# Extract the compass
p1.grob <- ggplotGrob(p1)

# cowplot::ggdraw() + 
#   cowplot::draw_plot(map.test2, 0, 0, 1, 1) +
#   cowplot::draw_plot(p1, 0.7, 0.2, 0.28, 0.28, scale = 1)

pdf("glyphTest2.pdf", paper = "USr", width = 10, height = 8)
result_plot2 + 
  inset(p1.grob, xmin = -70 - 7, xmax = -70 + 7,
        ymin = 29.5 - 7, ymax = 29.5 + 7) + 
  inset(legend, xmin = -102 - 10, xmax = -102 + 10,
        ymin = 51 - 1, ymax = 51 + 1)
dev.off()









