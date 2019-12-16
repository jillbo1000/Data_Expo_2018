# Map tests
library(googleVis)

# Example from Stack Exchange
# https://stackoverflow.com/questions/25530358/how-do-you-create-a-50-state-map-instead-of-just-lower-48
# Note that the default is to display the graph onto a web page
G4 <- gvisGeoChart(CityPopularity, locationvar='City', colorvar='Popularity',
                   options=list(region='US', height=350, 
                                displayMode='markers',
                                colorAxis="{values:[200,400,600,800],
                                colors:[\'red', \'pink\', \'orange',\'green']}")
                   ) 
plot(G4)
head(CityPopularity)

# Try some examples from the help page
G1b <- gvisGeoChart(Exports, locationvar='Country', colorvar='Profit',
                    options=list(projection="kavrayskiy-vii")) 

plot(G1b)

eq <- read.csv("http://earthquake.usgs.gov/earthquakes/feed/v0.1/summary/2.5_week.csv")
eq$loc=paste(eq$Latitude, eq$Longitude, sep=":")

library(XML)
url <- "http://en.wikipedia.org/wiki/List_of_countries_by_credit_rating"
x <- readHTMLTable(readLines(url), which=3)
levels(x$Rating) <- substring(levels(x$Rating), 4, 
                              nchar(levels(x$Rating)))
x$Ranking <- x$Rating
levels(x$Ranking) <- nlevels(x$Rating):1
x$Ranking <- as.character(x$Ranking)
x$Rating <- paste(x$Country, x$Rating, sep=": ")
#### Create a geo chart
G8 <- gvisGeoChart(x, "Country", "Ranking", hovervar="Rating",
                   options=list(gvis.editor="S&P", 
                                colorAxis="{colors:['#91BFDB', '#FC8D59']}"))
plot(G8)


# Tests on the weather data
setwd("C:/Users/jflun/Dropbox/USU/Advanced Graphics II/Data Expo/DataExpo2018")

weather = read.csv("Shiny Apps/TempSlider/data/ForecastSum.csv")
weather$latlong = paste(weather$latitude, weather$longitude, sep = ":")
weather$color = sample(c(0, 1), nrow(weather), replace = TRUE)

g1 = gvisGeoChart(weather, "latlong", sizevar = "color", colorvar = "color", 
                  options = list(region = "US", 
                                 colorAxis="{colors:['#91BFDB', '#FC8D59']}"))
plot(g1)

