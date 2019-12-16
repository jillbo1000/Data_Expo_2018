# Create correlation ellipse graph in ggplot. 

# Adapted from the corrplot and ellipse packages: 
# - https://cran.r-project.org/web/packages/corrplot/corrplot.pdf
# - https://cran.r-project.org/web/packages/ellipse/ellipse.pdf 

# Ellipse equation help:
# - http://math.etsu.edu/multicalc/prealpha/chap3/chap3-2/part4.htm

# Ellipses are calculated as follows:
# The orientation of the ellipse follows the line y = x or y = -x 
# depending on the sign of the correlation. The maginitude of the 
# correlation is inversely related to the minor axis. Thus a 
# perfect correlation (which is technically not allowed) will
# have a minor axis length = 0, and no correlation will have a 
# minor axis equal to the major axis (i.e. a perfect circle.)
# The size of each ellipse is adjusted "one the fly" to produce
# shapes with sizes similar to those provided in the
# corrplot() implementation. 

# Read in relevant packages. 
library(tidyverse)
library(RColorBrewer)
library(fiftystater)
library(mapproj)
library(latex2exp)

data("fifty_states") # reload map for new figure.
marAdj <- c(0, 0, 0, 0)
# Adjust the final plot margins as needed.
plot.margin = unit(marAdj, "cm")

locations <- read.csv("../Data/locationsFinal.csv")
locations$state = tolower(locations$state)
weatherSum <- read.csv("../Data/summary_city.csv") %>% 
  dplyr::select(-Cluster6, -name, -color, -ClustName)
# Calculate inverse of Brier Skill score as we want larger numbers
# to indicate more error. 
weatherSum$iBSS <- 1 - weatherSum$BSS 

corMethod <- "spearman"

# Change coordinates of Alaska and Hawaii to match the map we use.
longitude = c(-117.25, -108.50)
latitude = c(27.2, 27)
locations$latitude[locations$state == "alaska"] = latitude[1]
locations$latitude[locations$state == "hawaii"] = latitude[2]
locations$longitude[locations$state == "alaska"] = longitude[1]
locations$longitude[locations$state == "hawaii"] = longitude[2]

weatherSum$latitude[weatherSum$state == "Alaska"] = latitude[1]
weatherSum$latitude[weatherSum$state == "Hawaii"] = latitude[2]
weatherSum$longitude[weatherSum$state == "Alaska"] = longitude[1]
weatherSum$longitude[weatherSum$state == "Hawaii"] = longitude[2]

# Make a temporary data frame of coordinates so that everyting gets identically projected:
coordDF <- data.frame(long = c(fifty_states$long, weatherSum$longitude),
                      lat = c(fifty_states$lat, weatherSum$latitude),
                      id = c(rep(1, nrow(fifty_states)), rep(2, nrow(weatherSum))))

tcoords <- mapproject(coordDF$long, coordDF$lat, projection="mercator",
                      parameters=NULL, orientation=NULL)

fifty_states$long <- tcoords$x[coordDF$id == 1]
fifty_states$lat <- tcoords$y[coordDF$id == 1]

weatherSum$longitude2 <- tcoords$x[coordDF$id == 2]
weatherSum$latitude2 <- tcoords$y[coordDF$id == 2]

locs.sub <- locations %>% dplyr::select(AirPtCd, Cluster6, color)

weatherSum <- left_join(weatherSum, locs.sub, by = "AirPtCd")

weatherSum2 <- weatherSum
weatherSum2$Cluster6 <- 7
weatherSum2 <- bind_rows(weatherSum, weatherSum2)

# Calculate non-parametric cluster specific correlations.
latLonSum <- weatherSum2 %>% group_by(Cluster6) %>%
  summarize(long = mean(longitude2),
            lat = mean(latitude2)
  )

# Adjust individual lat/long coordinates as needed
# (particularly for those thrown off by Alaska and Hawaii)
# longitude adjustment
lonAdj <- c(0, 0.15, 0, -0.2, 0, 0, 0.45)
latAdj <- c(0, 0.05, 0, 0.05, 0.05, 0, -0.21)

latLonSum$long <- latLonSum$long + lonAdj
latLonSum$lat <- latLonSum$lat + latAdj


# Generate the coordinates for the ellipses for each correlation 
# and each cluster. Store as one common data frame.  
tempEllipse <- vector("list", 7)
tempEllipse.sub <- vector("list", 3)

grd <- 0.55
adj.lon <- grd*c(-1, 1, 1)
adj.lat <- grd*c(1, 1, -1)

# Change the size of the plots
arrayAdj <- 0.07 
# arrayAdj <- .035

# Combine the correlation information into a common data frame. 
# This data frame includes the equations for the ellipses. 
for(i in 1:length(tempEllipse)){
  temp <- cor(weatherSum2[weatherSum2$Cluster6 == i, c("mxT_mean_abs", "mnT_mean_abs", 
                                                       "iBSS")],
              method = corMethod)
  temp.p1 <- cor.test(weatherSum2[weatherSum2$Cluster6 == i, "mxT_mean_abs"],
                      weatherSum2[weatherSum2$Cluster6 == i, "mnT_mean_abs"],
                      method = corMethod)
  temp.p2 <- cor.test(weatherSum2[weatherSum2$Cluster6 == i, "mxT_mean_abs"],
                      weatherSum2[weatherSum2$Cluster6 == i, "iBSS"],
                      method = corMethod)
  temp.p3 <- cor.test(weatherSum2[weatherSum2$Cluster6 == i, "mnT_mean_abs"],
                      weatherSum2[weatherSum2$Cluster6 == i, "iBSS"],
                      method = corMethod)
  temp.p <- c(temp.p1$p.value, temp.p2$p.value, temp.p3$p.value)
  temp <- temp[c(2, 3, 6)]
  # If a correlation is exactly equal to 0, adjust to avoid numerical difficulties.
  temp[temp == 0] <- .001
  # Adjust the number of plot points depending on how narrow the ellipse is. 
  # The more narrow the ellipse, the more points we need to characterize the 
  # turns. 
  pointAdj <- c(.0025, .025, .05, .025)
  for(j in 1:length(tempEllipse.sub)){
    # Change the number of points based on the shape of the ellipse. 
    if(sign(temp[j]) < 0){
      tPointAdj <- pointAdj[c(2, 3, 4, 1)]
    }else{
      tPointAdj <- pointAdj
    }
    tempEllipse.sub[[j]] <- 
      data.frame(dRad = c(seq(0, 0.5, tPointAdj[1]*(1 - abs(temp[j])))*pi, 
                          seq(0.5, 1, tPointAdj[2]*(1 - abs(temp[j])))*pi,
                          seq(1, 1.5, tPointAdj[3]*(1 - abs(temp[j])))*pi,
                          seq(1.5, 2, tPointAdj[4]*(1 - abs(temp[j])))*pi),
                 Cluster = i,
                 CorType = j,
                 long = latLonSum$long[i] + arrayAdj*adj.lon[j],
                 lat = latLonSum$lat[i] + arrayAdj*adj.lat[j],
                 cor = temp[j],
                 pval = temp.p[j])
  }
  
  tempEllipse[[i]] <- bind_rows(tempEllipse.sub)
}

# Convert the polar coordinates of the ellipse to cartesian. 
# - http://math.etsu.edu/multicalc/prealpha/chap3/chap3-2/part4.htm
tempEllipse <- bind_rows(tempEllipse)
tempEllipse <- tempEllipse %>%
  mutate(c = sqrt(abs(cor)*(2-abs(cor))),
         r = (1-abs(cor))^2 / (1 - c*cos(dRad-sign(cor)*pi/4)),
         x = r*cos(dRad) - sqrt(c^2/2),
         y =  r*sin(dRad) - sign(cor)*sqrt(c^2/2))

adj <- tempEllipse %>% group_by(Cluster, CorType) %>% 
  summarize(adj = max(abs(x))/0.5)

tempEllipse <- left_join(tempEllipse, adj)

tempEllipse$group <- paste(tempEllipse$Cluster, tempEllipse$CorType, sep = ".")

# Determine the non-significant correlations. 
pvalSum <- tempEllipse %>% 
  group_by(Cluster, CorType) %>% 
  dplyr::select(Cluster, CorType, long, lat, pval) %>%
  slice(1) %>%
  filter(pval >= 0.05)

# Define color scheme
set1 <- c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", 
          "#ff7f00", "#f781bf", "gray50")
set1 <- set1[c(4, 2, 3, 6, 1, 5, 7)] # Reorder to match the original layout

# Test to see how the point distribution looks for all the ellipses.
plot(tempEllipse$x, tempEllipse$y, asp = 1)
abline(0, 0)
abline(v = 0)
abline(0, 1)
points(sqrt(tempEllipse$c^2/2), sign(tempEllipse$cor)*sqrt(tempEllipse$c^2/2), 
       col = "red", pch = 4)
points(-sqrt(tempEllipse$c^2/2), -sign(tempEllipse$cor)*sqrt(tempEllipse$c^2/2), 
       col = "red", pch = 4)
# CREATE LEGEND
#=============================================================================
# Change the size of the plots
legendAdj <- 0.03 

# Generate the coordinates for the ellipses for each correlation 
# and each cluster. Store as one common data frame.  
tempCor <- seq(-1, 1, 0.25)
# Slight adjustment to 0 to make plots consistent. 
tempCor[tempCor == 0] <- 0.0000001
tempLegend <- vector("list", length(tempCor))
for(j in 1:length(tempCor)){
  
  # Change the number of plotted points based on the type of ellipse. 
  if(tempCor[j] == 1){
    tdRad <- c(pi/4, 5*pi/4)
    # Adjust the value of b to avoid missing values. 
    lineAdj <- 0.0000001
  }else if(tempCor[j] == -1){
    tdRad <- c(3*pi/4, 7*pi/4)
    # Adjust the value of b to avoid missing values. 
    lineAdj <- -0.0000001
  }else{
    if(sign(tempCor[j]) < 0){
      tPointAdj <- pointAdj[c(2, 3, 4, 1)]
    }else{
      tPointAdj <- pointAdj
    }
    tdRad <- c(seq(0, 0.5, tPointAdj[1]*(1 - abs(tempCor[j])))*pi, 
               seq(0.5, 1, tPointAdj[2]*(1 - abs(tempCor[j])))*pi,
               seq(1, 1.5, tPointAdj[3]*(1 - abs(tempCor[j])))*pi,
               seq(1.5, 2, tPointAdj[4]*(1 - abs(tempCor[j])))*pi)
    # Adjust the value of b to avoid missing values. 
    lineAdj <- 0
  }
  
  tempLegend[[j]] <- data.frame(dRad = tdRad,
                                long = (j-1)*legendAdj,
                                b = (1 - abs(tempCor[j]))*sign(tempCor[j]) + 
                                  lineAdj, # Adjustment avoids trivial radii of 0.
                                id = j)
  
}

tempLegend <- bind_rows(tempLegend)
tempLegend$r[is.nan(tempLegend$r) || tempLegend$r == 0] <- 0.5

tempLegend <- tempLegend %>%
  mutate(c = sqrt(1 - b^2),
         r = b^2 / (1 - c*cos(dRad - sign(b)*pi/4)),
         x = r*cos(dRad) - sqrt(c^2/2),
         y =  r*sin(dRad) - sign(b)*sqrt(c^2/2))

# Note that max(x) and max(y) are identical by construction. No need to 
# define both. 
adj <- tempLegend %>% group_by(id) %>% summarize(adj = max(abs(x))/0.5)

tempLegend <- left_join(tempLegend, adj)
#=============================================================================


# Create the plot
#=============================================================================
locations$Title <- "Error Correlations by Cluster"
mapEllipse <- ggplot(tempEllipse) + 
  geom_map(data = locations, aes(map_id = state),
           fill = alpha("gray80", 0.5), color = alpha("gray60", 0.5),
           map = fifty_states) +
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  geom_polygon(aes(x = arrayAdj*x/adj + long, y = arrayAdj*y/adj + lat, 
                   group = group, fill = factor(Cluster))) +
  # Add the reference grid lines.
  annotate("segment", 
           x = c(latLonSum$long - 2*arrayAdj*grd,
                 latLonSum$long - 2*arrayAdj*grd,
                 latLonSum$long, 
                 latLonSum$long,
                 latLonSum$long + 2*arrayAdj*grd,
                 latLonSum$long - 2*arrayAdj*grd), 
           y = c(latLonSum$lat, 
                 latLonSum$lat + 2*arrayAdj*grd,
                 latLonSum$lat - 2*arrayAdj*grd,
                 latLonSum$lat - 2*arrayAdj*grd,
                 latLonSum$lat - 2*arrayAdj*grd,
                 latLonSum$lat),
           xend = c(latLonSum$long + 2*arrayAdj*grd,
                    latLonSum$long + 2*arrayAdj*grd,
                    latLonSum$long + 2*arrayAdj*grd,
                    latLonSum$long,
                    latLonSum$long + 2*arrayAdj*grd,
                    latLonSum$long - 2*arrayAdj*grd), 
           yend = c(latLonSum$lat,
                    latLonSum$lat + 2*arrayAdj*grd,
                    latLonSum$lat - 2*arrayAdj*grd,
                    latLonSum$lat + 2*arrayAdj*grd,
                    latLonSum$lat + 2*arrayAdj*grd,
                    latLonSum$lat + 2*arrayAdj*grd),
           col = alpha("black", 1), lwd = 1) + 
  # Add the variable labels
  annotate("text", 
           x = latLonSum$long[7] + c(-3.75, -1.75, -1.75, 1)*grd*arrayAdj, 
           y = latLonSum$lat[7] + c(1, 2.4, -1, 2.4)*grd*arrayAdj,
           label = c("Max Temp", "Min Temp", "Min Temp", "Precip"),
           size = 10) + 
  # Add the "combined" title
  annotate("text", x = latLonSum$long[7], 
           y = latLonSum$lat[7] + 3.5*grd*arrayAdj,
           label = c("Combined"), size = 14) + 
  # Add the significance annotation. 
  annotate("point", x = 0.09, y = .98, pch = 4, size = 3, stroke = 2) +
  annotate("text", x = 0.1, y = .98, 
           label = TeX("Non-significant correlation ($\\alpha = 0.05$)", 
                       output = 'character'), 
           hjust = -0.05, 
           parse = TRUE, size = 10) +
  # Add "x" to all correlations that are not significant. 
  # Stroke option help:
  # - https://ggplot2.tidyverse.org/reference/geom_point.html
  annotate("point", x = pvalSum$long, pvalSum$lat, pch = 4, size = 3, stroke = 2,
           color = alpha("black", 0.5)) + 
  # Add the legend:
  geom_polygon(data = tempLegend, 
               aes(x = legendAdj*x/adj + long + (id-1)*(grd-0.53) - 0.1, 
                   y = legendAdj*y/adj + 0.4, group = factor(id)),
               inherit.aes = FALSE,
               fill = "black", color ="black", lwd = 1) + 
  annotate("text", 
           x = unique(tempLegend$long)[c(1, 3, 5, 7, 9)] + 
             seq(0, 8, 2)*(grd-0.53) - 0.1, y = -0.06 + 0.4, 
           label = c("-1", "-0.5",  "0", "0.5", "1"), 
           size = 12) + 
  annotate("text", 
           x = unique(tempLegend$long)[5] + 
             4*(grd-0.53) - 0.1, y = 0.05 + 0.4, 
           label = "Correlation", size = 14) + 
  annotate("point", 
           x = unique(tempLegend$long) + 
             0:8*(grd-0.53) - 0.1, y = -0.03 + 0.4, 
           pch = "|", size = 6) + 
  annotate("segment", 
           x = unique(tempLegend$long) +
             0*(grd-0.53) - 0.1, y = -0.03 + 0.4, 
           xend = unique(tempLegend$long) + 
             8*(grd-0.53) - 0.1, yend = -0.03 + 0.4) + 
  coord_fixed() + # Ensures integrity of shapes. 
  scale_fill_manual(values = alpha(set1, 0.5)) + 
  #facet_grid(~Title) + 
  theme(panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        #strip.text = element_text(size = 18),
        legend.position = "none",
        plot.margin = unit(marAdj, "cm")) 
#=============================================================================

#mapEllipse
#pdf("../images/final/corGlyphMap2.pdf", width = 20, height = 12)
#mapEllipse
#dev.off()

#pdf("../images/final/corGlyphMap2_slides.pdf", width = 20, height = 12)
#mapEllipse
#dev.off()


#=============================================================================
# First, create a set of boxes that we can color differently. 
tbox.x <- c(0, 0, 5, 5, 0)
tbox.y <- c(0, 1, 1, 0, 0)

# Now create a series of 6 boxes. 
boxes <- data.frame(x = c(tbox.x, tbox.x+5, tbox.x+10, tbox.x+15, 
                          tbox.x+20, tbox.x+25),
                    y = rep(tbox.y, 6),
                    group = c(rep(1, 5), rep(2, 5), rep(3, 5), 
                              rep(4, 5), rep(5, 5), rep(6, 5)))

legend <- ggplot(boxes, aes(x = x, y = y)) + 
  geom_polygon(aes(fill = factor(group)), color = "white") + 
  coord_fixed() +
  scale_color_manual(values = c("#f781bf", "#4daf4a", "#377eb8",  
                                "#e41a1c", "#984ea3", "#ff7f00")) + 
  scale_fill_manual(values = c("#f781bf", "#4daf4a", "#377eb8", 
                               "#e41a1c", "#984ea3",  "#ff7f00")) + 
  annotate("text", x = 2.5 + c(0, 5, 10, 15, 20, 25), 
           y = rep(0.5, 6), 
           label = c("Cali-Florida", "Southeast", "Northeast", 
                     "Intermountain West", "Midwest", "Southwest"), 
           color = "white", size = c(10, 10, 10, 7.5, 10, 10)) + 
  theme(legend.position = "none",
        panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())
#=============================================================================


pdf("../images/final/paper/corGlyphMapNew.pdf", width = 20, height = 12)
gridExtra::grid.arrange(grobs = list(ggplotGrob(mapEllipse), ggplotGrob(legend)),
                        heights = c(0.9, 0.1))
dev.off()


