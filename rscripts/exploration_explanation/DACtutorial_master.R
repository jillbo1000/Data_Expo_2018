# Tutorial for how to create polygon-based glyph plots in R. 
# Author: Brennan Bean
# Date:   9-11-2018

# First, load some packages and data. 
#=============================================================================
library(ggmap)
library(tidyverse)
library(sp)
library(RColorBrewer)
library(grid)
library(gridExtra)
library(gtable)
library(fiftystater)
library(reshape2)
library(mapproj)
# library(cowplot) # don't load explicitly as it changes default ggplot themes

# Project the 50 states and use the coordinates. 
data("fifty_states") # this line is optional due to lazy data loading

# Load summarized data set (by month)
weatherSum <- read.csv("../../data/summary_city_month.csv")
locations <- read.csv("../../data/locationsFinal.csv")

# Change the name of the states to match the fiftystater package format. 
locations$stateABB <- "none"
for(i in 1:nrow(locations)){locations$stateABB[i] <- state.abb[grep(locations$state[i], state.name)]}
locations$state = tolower(locations$state)

# Set up the color pallete we will use. 
set1 <- c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#f781bf")
set1 <- set1[c(4, 2, 3, 6, 1, 5)] # Reorder to match the original layout
#=============================================================================

# Next, plot a map of the US using the fifty stater map and add points 
# for the locations. 
#=============================================================================
ggplot(locations, aes(map_id = state)) +
  # map points to the fifty_states shape data
  geom_map(
    fill = scales::alpha("gray80", 0.5), color = scales::alpha("gray60", 0.5), 
    map = fifty_states) +
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  geom_point(aes(x = longitude, y = latitude, 
                 color = factor(Cluster6)), size = 2) +
  scale_color_manual(values = set1)

# Whats wrong with this plotting window? How might we fix it? 
#=============================================================================


# Re draw the plot with Alaska and Hawaii adjustment. 
#=============================================================================
# Adjust the location of Alaska and Hawaii
longitude = c(-117.25, -108.50)
latitude = c(27.2, 27)
locations$latitude[locations$state == "alaska"] = latitude[1]
locations$latitude[locations$state == "hawaii"] = latitude[2]
locations$longitude[locations$state == "alaska"] = longitude[1]
locations$longitude[locations$state == "hawaii"] = longitude[2]

weatherSum$latitude[weatherSum$state == "alaska"] = latitude[1]
weatherSum$latitude[weatherSum$state == "hawaii"] = latitude[2]
weatherSum$longitude[weatherSum$state == "alaska"] = longitude[1]
weatherSum$longitude[weatherSum$state == "hawaii"] = longitude[2]

ggplot(locations, aes(map_id = state)) +
  # map points to the fifty_states shape data
  geom_map(
    fill = scales::alpha("gray80", 0.5), color = scales::alpha("gray60", 0.5), 
    map = fifty_states) +
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  geom_point(aes(x = longitude, y = latitude, 
                 color = factor(Cluster6)), size = 2) +
  scale_color_manual(values = set1) 

# How can we make the map of the US look less distorted? 
#=============================================================================

# Fix map distortions. 
#=============================================================================
ggplot(locations, aes(map_id = state)) +
  # map points to the fifty_states shape data
  geom_map(
    fill = scales::alpha("gray80", 0.5), color = scales::alpha("gray60", 0.5), 
    map = fifty_states) +
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  geom_point(aes(x = longitude, y = latitude, 
                 color = factor(Cluster6)), size = 2) +
  scale_color_manual(values = set1) + 
  coord_map()
#=============================================================================


# Using this map, lets now plot a circle in the geographical center of the US
# using polar coordinates. 
#=============================================================================
# Create a data frame that creates a circle of radius one.
circSeq <- seq(0, 2*pi, length.out = 500)
circDF <- data.frame(x = cos(circSeq), y = sin(circSeq))

# Center the circle using coordinates. 
# (Geographical center from wikipedia)
circDF$x <- circDF$x - 98.5833
circDF$y <- circDF$y + 39.8333
  
ggplot(locations, aes(map_id = state)) +
  # map points to the fifty_states shape data
  geom_map(
    fill = scales::alpha("gray80", 0.5), color = scales::alpha("gray60", 0.5), 
    map = fifty_states) +
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  geom_point(aes(x = longitude, y = latitude, 
                 color = factor(Cluster6)), size = 2) +
  scale_color_manual(values = set1) + 
  coord_map() + 
  geom_path(data = circDF, aes(x = x, y = y), col = "black", inherit.aes = FALSE)
#=============================================================================


# Now, replicate this circle and center at each location. Have the color of 
# each location match the color of the point. 
#=============================================================================
# Create a data frame that creates a circle of radius one.
circSeq <- seq(0, 2*pi, length.out = 500)
circDF <- data.frame(x = rep(cos(circSeq), nrow(locations)),
                     y = rep(sin(circSeq), nrow(locations)),
                     id = rep(1:nrow(locations), each = length(circSeq)))

circDF$x <- circDF$x + locations$longitude[circDF$id]
circDF$y <- circDF$y + locations$latitude[circDF$id]
circDF$Cluster6 <- locations$Cluster6[circDF$id]

ggplot(locations, aes(map_id = state)) +
  # map points to the fifty_states shape data
  geom_map(
    fill = scales::alpha("gray80", 0.5), color = scales::alpha("gray60", 0.5), 
    map = fifty_states) +
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  geom_point(aes(x = longitude, y = latitude, 
                 color = factor(Cluster6)), size = 1) +
  scale_color_manual(values = set1) + 
  coord_map() + 
  geom_path(data = circDF, aes(x = x, y = y, color = factor(Cluster6), 
                               group = factor(id)), inherit.aes = FALSE)

# What do you notice about the shape of the circles? 
# (Think about the projection of a map...)
#=============================================================================


# Ignoring the projection issue create variable lengths circles by adjusting 
# the radius between 0 and 1. 
#=============================================================================
# Create a data frame that creates a circle of radius one.
circSeq <- seq(0, 2*pi, length.out = 500)
r <- (sin(5*circSeq) + 1 / 2)
circDF <- data.frame(x = rep(r*cos(circSeq), nrow(locations)),
                     y = rep(r*sin(circSeq), nrow(locations)),
                     id = rep(1:nrow(locations), each = length(circSeq)))

circDF$x <- circDF$x + locations$longitude[circDF$id]
circDF$y <- circDF$y + locations$latitude[circDF$id]
circDF$Cluster6 <- locations$Cluster6[circDF$id]

ggplot(locations, aes(map_id = state)) +
  # map points to the fifty_states shape data
  geom_map(
    fill = scales::alpha("gray80", 0.5), color = scales::alpha("gray60", 0.5), 
    map = fifty_states) +
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  geom_point(aes(x = longitude, y = latitude, 
                 color = factor(Cluster6)), size = 1) +
  scale_color_manual(values = set1) + 
  coord_map() + 
  geom_path(data = circDF, aes(x = x, y = y, color = factor(Cluster6), 
                               group = factor(id)), inherit.aes = FALSE)

#=============================================================================

