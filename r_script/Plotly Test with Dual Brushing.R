setwd("C:/Users/jflun/Dropbox/USU/Advanced Graphics II/Data Expo/DataExpo2018")

library(plotly)
library(leaflet)
library(crosstalk)

# The developmental version of leaflet has to be used for this. 
# devtools::install_github("rstudio/leaflet#346")
# This code produces a histogram and a map. Either one can 
# be brushed to see how it affects the other plot.

weather = read.csv("Data/ForecastSum.csv")
head(weather)

# The following is a crosstalk command that takes the dataframe and
# creates an object that can be passed to crosstalk-compatable
# widgets in place of a dataframe. Each SharedData$new(...)
# makes a new group of widgets that link to each other, but not
# to widgets in other groups. A SharedData object from Shiny can
# be used as well for non-widget visualizations such as
# ggplot2 plots.

# The following is the simplest example with the weather data. It 
# uses the default settings. The brushing is a little glitch.

SD = SharedData$new(weather)
options(persistent = TRUE)

p <- plot_ly(SD, x = ~mxT_mean_abs, y = ~mnT_mean_abs) %>% 
  add_markers(alpha = 0.5) %>%
  highlight("plotly_selected", dynamic = TRUE)

map <- leaflet(SD) %>% 
  addTiles() %>% 
  addCircles()

bscols(widths = c(6, 6), p, map)

# Change some of the settings. Brushing is still glitchy. It works with the 
# scatterplot, but you have to reload the graph if you want to remove previous
# brushing. I cannot get brushing to work intuitively with the map. You have to
# click on the little square and then it brings up a rectangle. As with the 
# scatterplot, once the rectangle selects a point it stays selected, even
# when you resize the rectangle. The only way to get the points you want 
# is to zoom into the area you want and reposition the map. Then the
# rectangle will pop-up where you want it. At that point you can zoom the 
# map in and out to see more of it, but the process is not intuitive. 

SD = SharedData$new(weather)
options(persistent = TRUE)

p <- plot_ly(SD, x = ~mxT_mean_abs, y = ~mnT_mean_abs) %>% 
  add_markers(alpha = 0.5) %>%
  highlight("plotly_selected", dynamic = TRUE)

# I was hoping to make the circles on the map smaller by decreasing the
# radius, but the change in radius size seems to make no difference.
map <- leaflet(SD) %>% 
  addTiles() %>% 
  addCircles(radius = 1)

# bscols brings in all of Bootstrap. This is a css web layout thing 
# that dictates where objects show up on a page. This allows HTML 
# elements to be displayed side-by-side. The width of a BS page is 12
# so the following gives equal space to the two plots.
bscols(widths = c(6, 6), p, map)

###############################################################################

# This next bit tries to link a histogram with the map. When I brush the 
# histogram it will not highlight anything on them map. However, when I 
# hightlight the map it will highlight the appropriate locations on the 
# histogram.

SD = SharedData$new(weather)
options(persistent = TRUE)

h <- plot_ly(SD, x = ~mxT_mean_abs, type = "histogram") %>% 
  highlight("plotly_selected", dynamic = TRUE)

# I was hoping to make the circles on the map smaller by decreasing the
# radius, but the change in radius size seems to make no difference.
map <- leaflet(SD) %>% 
  addTiles() %>% 
  addCircles()

# bscols brings in all of Bootstrap. This is a css web layout thing 
# that dictates where objects show up on a page. This allows HTML 
# elements to be displayed side-by-side. The width of a BS page is 12
# so the following gives equal space to the two plots.
bscols(widths = c(6, 6), h, map)

# Here is another attempt based on the R bloggers post
# https://www.r-bloggers.com/visualizing-geo-spatial-data-with-sf-and-plotly/
# I had to set up a mapview account and get a token in order to use the
# mapping function here. The Sys.setenv function holds the token 
# I got from my account.
# I can get the graphs to show up and pretend to work, but brushing one does
# not change the other one. It also shows Africa and I have to drag the map 
# around to see the U.S. The points are there, but for some reason
# the map does not appear where the data are. 

library(mapview)
Sys.setenv('MAPBOX_TOKEN' = 'pk.eyJ1IjoiamlsbGx1bmRlbGwiLCJhIjoiY2pnMzhlOGZjMXMyNzJxbnhucG1ucGlkbSJ9.yQSXQFy8EuFx78tbPQpDWQ')

SD = SharedData$new(weather)

bscols(
  plot_mapbox(SD, x = ~longitude, y = ~latitude) %>%
    highlight(dynamic = TRUE, persistent = TRUE),
  plot_ly(SD, x = ~mxT_mean_abs) %>% 
    add_histogram(xbins = list(start = 0, end = 10, size = 0.5)) %>%
    layout(barmode = "overlay") %>% 
    highlight("plotly_selected", persistent = TRUE)
)

