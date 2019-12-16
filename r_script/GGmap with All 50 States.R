setwd("C:/Users/jflun/Dropbox/USU/Advanced Graphics II/Data Expo/DataExpo2018")

library(dplyr)
library(shiny)
library(rgl)
library(maps)
library(mapdata)
library(RColorBrewer)
library(ggplot2)
library(plotly)
library(fiftystater)

data("fifty_states")

crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)

weather = read.csv("Data/ForecastSum.csv")
w2 = weather

longitude = c(-117.25, -108.50)
latitude = c(27.2, 27)
state = c("Alaska", "Hawaii")

w2$latitude[w2$state == "Alaska"] = latitude[1]
w2$latitude[w2$state == "Hawaii"] = latitude[2]
w2$longitude[w2$state == "Alaska"] = longitude[1]
w2$longitude[w2$state == "Hawaii"] = longitude[2]

w2$state = tolower(w2$state)

p <- ggplot(w2, aes(map_id = state)) + 
  # map points to the fifty_states shape data
  geom_map(fill = "gray80", map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme(legend.position = "bottom", 
        panel.background = element_blank()) +
  borders("state")

p2 = p + geom_point(aes(x = longitude, y = latitude), data = w2)

ggplotly(p2)

# Figure out where to put Alaska and Hawaii

longitude = c(-117.25, -108.50)
latitude = c(27.2, 27)
state = c("Alaska", "Hawaii")
test = as.data.frame(cbind(longitude, latitude))
test$state = state

w2 = weather
w2$latitude[w2$state == "Alaska"] = latitude[1]
w2$latitude[w2$state == "Hawaii"] = latitude[2]
w2$longitude[w2$state == "Alaska"] = longitude[1]
w2$longitude[w2$state == "Hawaii"] = longitude[2]

p + geom_point(aes(x = longitude, y = latitude), data = w2) 
  geom_point(aes(x = longitude, y = latitude), data = test)


#######################################################################

ids <- factor(c("1.1", "2.1", "1.2", "2.2", "1.3", "2.3"))

values <- data.frame(
  id = ids,
  value = c(3, 3.1, 3.1, 3.2, 3.15, 3.5)
)

positions <- data.frame(
  id = rep(ids, each = 4),
  x = c(2, 1, 1.1, 2.2, 1, 0, 0.3, 1.1, 2.2, 1.1, 1.2, 2.5, 1.1, 0.3,
        0.5, 1.2, 2.5, 1.2, 1.3, 2.7, 1.2, 0.5, 0.6, 1.3),
  y = c(-0.5, 0, 1, 0.5, 0, 0.5, 1.5, 1, 0.5, 1, 2.1, 1.7, 1, 1.5,
        2.2, 2.1, 1.7, 2.1, 3.2, 2.8, 2.1, 2.2, 3.3, 3.2)
)

ggplot(values) +
  geom_map(aes(map_id = id), map = positions) +
  expand_limits(positions)
ggplot(values, aes(fill = value)) +
  geom_map(aes(map_id = id), map = positions) +
  expand_limits(positions)
ggplot(values, aes(fill = value)) +
  geom_map(aes(map_id = id), map = positions) +
  expand_limits(positions) + ylim(0, 3)

# Better example
crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)
crimesm <- reshape2::melt(crimes, id = 1)
if (require(maps)) {
  states_map <- map_data("state")
  ggplot(crimes, aes(map_id = state)) +
    geom_map(aes(fill = Murder), map = states_map) +
    expand_limits(x = states_map$long, y = states_map$lat)
  
  last_plot() + coord_map()
  ggplot(crimesm, aes(map_id = state)) +
    geom_map(aes(fill = value), map = states_map) +
    expand_limits(x = states_map$long, y = states_map$lat) +
    facet_wrap( ~ variable)
}





