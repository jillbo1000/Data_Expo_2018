# Create correlation matrix.
# Read in cluster data.

library(ggmap)
library(tidyverse)
library(sp)
library(RColorBrewer)
library(grid)
library(gtable)
library(fiftystater)
library(reshape2)
library(mapproj)
library(ggforce)
library(latex2exp)

data("fifty_states") # reload map for new figure.

locations <- read.csv("../Data/locationsFinal.csv")
weatherSum <- read.csv("../Data/summary_city.csv")

corMethod <- "spearman"

# Change coordinates of Alaska and Hawaii to match the map we use.
longitude = c(-117.25, -108.50)
latitude = c(27.2, 27)
locations$latitude[locations$state == "Alaska"] = latitude[1]
locations$latitude[locations$state == "Hawaii"] = latitude[2]
locations$longitude[locations$state == "Alaska"] = longitude[1]
locations$longitude[locations$state == "Hawaii"] = longitude[2]

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
corSum <- weatherSum2 %>% group_by(Cluster6) %>%
  summarize(cor1 = cor(mxT_mean_abs, mnT_mean_abs, method = corMethod),
            cor1.p = cor.test(mxT_mean_abs, mnT_mean_abs, method = corMethod)$p.value,
            cor2 = cor(mxT_mean_abs, 1 - BSS, method = corMethod),
            cor2.p = cor.test(mxT_mean_abs, 1 - BSS, method = corMethod)$p.value,
            cor3 = cor(mnT_mean_abs, 1 - BSS, method = corMethod),
            cor3.p = cor.test(mnT_mean_abs, 1 - BSS, method = corMethod)$p.value,
            long = mean(longitude2),
            lat = mean(latitude2)
  )

# Adjust individual lat/long coordinates as needed
# (particularly for those thrown off by Alaska and Hawaii)
# longitude adjustment
lonAdj <- c(0, 0.15, 0, -0.2, 0, 0, 0.45)
latAdj <- c(0, 0.05, 0, 0.05, 0.05, 0, -0.21)

corSum$long <- corSum$long + lonAdj
corSum$lat <- corSum$lat + latAdj

corSum.melt <- reshape2::melt(corSum, measure.vars = c("cor1", "cor2", "cor3"))
# Create one set of correlations for the melted dataset.
corSum.melt$pval <- corSum.melt$cor1.p
corSum.melt$pval[corSum.melt$variable == "cor2"] <- corSum.melt$cor2.p[corSum.melt$variable == "cor2"]
corSum.melt$pval[corSum.melt$variable == "cor3"] <- corSum.melt$cor3.p[corSum.melt$variable == "cor3"]
corSum.melt$thresh <- corSum.melt$pval < 0.05

corSum.melt$dRad <- pi/4
corSum.melt$dRad[corSum.melt$variable == "cor2"] <- pi/2
corSum.melt$dRad[corSum.melt$variable == "cor3"] <- 3*pi/4
rad <- 0.1

#corSum.melt$value2 <- corSum.melt$value / max(corSum.melt$value)
corSum.melt$value2 <- corSum.melt$value

# Calculate overall correlations.
cor1.full <- cor(weatherSum$mxT_mean_abs, weatherSum$mnT_mean_abs, method = corMethod)
cor1.full.test <- cor.test(weatherSum$mxT_mean_abs, weatherSum$mnT_mean_abs, method = corMethod)
cor2.full <- cor(weatherSum$mxT_mean_abs, 1 - weatherSum$BSS, method = corMethod)
cor3.full <- cor(weatherSum$mnT_mean_abs, 1- weatherSum$BSS, method = corMethod)


locations$state = tolower(locations$state)
corSum.melt$Type <- "Error Correlations By Cluster"

set1 <- c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#f781bf", "black")
set1 <- set1[c(4, 2, 3, 6, 1, 5, 7)] # Reorder to match the original layout

arType <- c("open", "closed")

# Help with latex annotations:
# - https://stackoverflow.com/questions/12514612/how-to-annotate-ggplot-with-latex/40528496
# - ftp://cran.r-project.org/pub/R/web/packages/latex2exp/vignettes/using-latex2exp.html
mapCor <- ggplot(corSum.melt) +
  # map points to the fifty_states shape data
  geom_map(data = locations, aes(map_id = state),
           fill = alpha("gray90", 0.5), color = alpha("gray80", 0.5),
           map = fifty_states) +
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_fixed() +
  # Map the points, lines, symbols, and circles that represent the correlation
  geom_point(aes(x = long, y = lat, color = factor(Cluster6)),
             size = 0.1) +
  geom_circle(aes(x0 = long, y0 = lat, r = rad, color = factor(Cluster6))) +
  geom_segment(aes(color = factor(Cluster6), x = long - rad, xend = long + rad, y = lat, yend = lat),
               lty = 1, alpha = 0.4) +
  geom_segment(aes(x = long, y = lat, color = factor(Cluster6),
                   xend = long+rad*cos(dRad), yend = lat+rad*sin(dRad)),
               lty = 2, alpha = 0.4) +
  geom_segment(aes(x = long, y = lat, color = factor(Cluster6),
                   xend = long-rad*cos(dRad), yend = lat-rad*sin(dRad)),
               lty = 2, alpha = 0.4) +
  geom_segment(aes(x = long, y = lat, color = factor(Cluster6),
                   xend = long+value2*rad*cos(dRad), yend = lat+value2*rad*sin(dRad)),
               arrow = arrow(type = arType[as.numeric(corSum.melt$thresh) + 1],
                             length = unit(0.25, "cm"))) +
  scale_color_manual(values = set1) +
  # Create the manual legend for the black circle. 
  annotate("text", x = corSum$long[7] - rad - 0.02, y = corSum$lat[7] + 0.35*rad,
           label = "(+)", size = 5) +
  annotate("text", x = corSum$long[7] - rad - 0.02, y = corSum$lat[7] - 0.35*rad,
           label = "(-)", size = 5) +
  annotate("text", x = corSum$long[7] + rad + 0.02, y = corSum$lat[7] + 0.35*rad,
           label = "(+)", size = 5) +
  annotate("text", x = corSum$long[7] + rad + 0.02, y = corSum$lat[7] - 0.35*rad,
           label = "(-)", size = 5) +
  annotate("text", x = corSum$long[7] + c(-1, 1)*1.2*rad, y = rep(corSum$lat[7], 2),
           label = c(-1, 1), size = 4) +
  annotate("text", x = corSum$long[7], y = corSum$lat[7] + rad + 0.05,
           label = "Combined", size = 5) +
  annotate("text", x = corSum$long[7] + c(1.4, 1.2, 1.4)*rad*cos(c(pi/4, pi/2, 3*pi/4)), 
           y = corSum$lat[7] +  c(1.4, 1.2, 1.4)*rad*sin(c(pi/4, pi/2, 3*pi/4)),
           label = c("Max Temp", "Max Temp", "Min Temp"), col = "black") +
  annotate("text", x = corSum$long[7] -  c(1.4, 1.2, 1.4)*rad*cos(c(pi/4, pi/2, 3*pi/4)), 
           y = corSum$lat[7] -  c(1.4, 1.2, 1.4)*rad*sin(c(pi/4, pi/2, 3*pi/4)),
           label = c("Min Temp", "Precip", "Precip"), col = "black") +
  # Create the manual legend for significance/non-significance
  geom_segment(x = 0.25, y = .99, xend = 0.25, yend = .990001,
               arrow = arrow(type = "open", length = unit(0.25, "cm"))) +
  geom_segment(x = 0.25, y = .96, xend = 0.25, yend = .960001,
               arrow = arrow(type = "closed", length = unit(0.25, "cm"))) +
  annotate("text", x = 0.26, y = .96,
           label ="p < 0.05", hjust = -.1, vjust = .9, 
           parse = TRUE) + 
  annotate("text", x = 0.26, y = .99, vjust = .9,
           label = TeX("$p \\geq 0.05$", output = 'character'), hjust = -.1, 
           parse = TRUE) +
  facet_grid(~Type) +
  theme(legend.position = "none",
        panel.background = element_rect(fill = NA, color = "gray"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        strip.text = element_text(size = 18))


mapCor

pdf("../images/glyphPlots/corGlyphMap.pdf", paper = "USr", width = 20, height = 16)
mapCor
dev.off()




# # Plot 1 - Simple plot, sacrifices spatial relationships.
# p1 <- ggplot(corSum.melt[corSum.melt$Cluster6 < 7, ]) +
#   geom_bar(stat = "identity", position = "dodge",
#            aes(x = factor(variable), y = value, fill = factor(Cluster6))) +
#   scale_y_continuous(limits = c(-1, 1)) +
#   scale_fill_manual(values = set1) +
#   theme(legend.position = "none")
# 
# 
# # Plot 2 - Relies on color.
# library(corrgram)
# library(corrplot)
# corrgram(corSum[, c(2, 4, 6)])
# 
# for(i in 1:6){
#   par(mfrow = c(3, 2))
#   corrgram(weatherSum[weatherSum$Cluster6 == i,
#                       c(33, 35, 36)],
#            lower.panel=corrgram::panel.ellipse,
#            upper.panel = panel.conf)
# }
# 
# 
# for(i in 1:6){
#   par(mfrow = c(3, 2))
#   corrgram(weatherSum[weatherSum$Cluster6 == i,
#                       c(33, 35, 36)],
#            lower.panel=corrgram::panel.ellipse,
#            upper.panel = panel.conf)
# }
# 
# 
# temp <- cor(weatherSum[weatherSum$Cluster6 == 1, c(33, 35, 36)])
# cor.mtest <- function(mat, ...) {
#   mat <- as.matrix(mat)
#   n <- ncol(mat)
#   p.mat<- matrix(NA, n, n)
#   diag(p.mat) <- 0
#   for (i in 1:(n - 1)) {
#     for (j in (i + 1):n) {
#       tmp <- cor.test(mat[, i], mat[, j], ...)
#       p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
#     }
#   }
#   colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
#   p.mat
# }
# temp.p <- cor.mtest(weatherSum[weatherSum$Cluster6 == 1, c(33, 35, 36)])
# 
# # par(mfrow = c(2, 3))
# tplots <- vector("list", 6)
# for(i in 1:6){
#   temp <- cor(weatherSum[weatherSum$Cluster6 == i, c(33, 35, 36)])
#   temp.p <- cor.mtest(weatherSum[weatherSum$Cluster6 == i, c(33, 35, 36)])
#   tplots[[i]] <- corrplot(temp, type="upper", order="hclust", col = c("gray", "white"),
#                           bg = unique(weatherSum$color)[i],
#                           p.mat = temp.p, sig.level = 0.05, diag = FALSE,
#                           tl.srt = 0,
#                           tl.col = "black")
# }
# 
# pdf("../images/glyphPlots/correloTest.pdf")
# par(mfrow = c(3, 2))
# par(mar = c(0, 0, 0, 0))
# tplots2 <- vector("list", 6)
# for(i in 1:6){
#   temp <- cor(weatherSum[weatherSum$Cluster6 == i, c(33, 35, 36)])
#   temp.p <- cor.mtest(weatherSum[weatherSum$Cluster6 == i, c(33, 35, 36)])
#   corrplot(temp, type="upper", method = "ellipse", order="hclust", col = levels(weatherSum$color)[i],
#                            bg = "white",
#                            p.mat = temp.p, sig.level = 0.05, diag = FALSE, 
#                            cl.cex = 1.5, rect.lwd = 20,
#                            tl.srt = 0,
#                            tl.col = "black")
# }
# 
# dev.off()
# 
# 
# library(ellipse)
# ellipse::plotcorr(temp, outline = TRUE, col = "blue", numbers = FALSE,
#          type = c("upper"), bty = "n", axes = FALSE,
#          xlab = "", ylab = "", asp = 1,
#          cex.lab = par("cex.lab"), cex = 0.75*par("cex"),
#          mar = 0.1 + c(2,2,4,2))
# 
# 
# # Ellipse equation help:
# # - http://math.etsu.edu/multicalc/prealpha/chap3/chap3-2/part4.htm
# a = 1; b = -.1;
# a = a/2; b = b/2
# c = sqrt(a^2 - b^2)
# ep = c / a; pm = b^2 / a
# 
# tempEllipse <- vector("list", 7)
# tempEllipse.sub <- vector("list", 3)
# for(i in 1:length(tempEllipse)){
# temp <- cor(weatherSum[weatherSum$Cluster6 == i, c(33, 35, 36)])
# temp <- temp[c(2, 3, 6)]
# for(j in 1:length(tempEllipse.sub)){
# tempEllipse.sub[[j]] <- data.frame(dRad = c(seq(0, 0.25, .005*abs(temp[j]))*pi, 
#                                    seq(0.25, 1.75, .05*abs(temp[j]))*pi, 
#                                    seq(1.75, 2, .005*abs(temp[j]))*pi),
#                                Cluster = i,
#                                Cor = j)
# }
# 
# tempEllipse[[i]] <- bind_rows(tempEllipse.sub[[j]])
# }
# 
# tempEllipse <- bind_rows(tempEllipse)
# 
# tempEllipse$r <- pm / (1 - ep*cos(tempEllipse$dRad))
# tempEllipse$x <-  tempEllipse$r*cos(sign(b)*pi/4 + tempEllipse$dRad) - sqrt(c^2/2)
# tempEllipse$y <-  tempEllipse$r*sin(sign(b)*pi/4 + tempEllipse$dRad) - sign(b)*sqrt(c^2/2)
# 
# adj <- max(tempEllipse$x) / 0.5
# 
# # Stroke option help:
# # - https://ggplot2.tidyverse.org/reference/geom_point.html
# test <- ggplot(tempEllipse) + 
#   geom_polygon(aes(x = x/adj, y = y/adj)) +
#   coord_fixed() + 
#   theme(panel.background = element_rect(fill = NA, color = "gray"),
#         panel.grid = element_blank(),
#         axis.title = element_blank(),
#         #axis.text = element_blank(),
#         #axis.ticks = element_blank(),
#         legend.position = "none") + 
#   #lims(x = c(-.501, .501), y = c(-.501, .501)) + 
#   geom_point(x = 0, y = 0, pch = 4, col = "gray", size = 6, stroke = 5)
# 
# test
# #test <- ggplotGrob(test)
# 
# 
# mapCor <- ggplot(corSum.melt) +
#   # map points to the fifty_states shape data
#   geom_map(data = locations, aes(map_id = state),
#            fill = alpha("gray90", 0.5), color = alpha("gray80", 0.5),
#            map = fifty_states) +
#   expand_limits(x = fifty_states$long, y = fifty_states$lat) +
#   coord_fixed() +
#   annotation_custom(test, xmin = 0, xmax = 0.5, ymin = 0, ymax = 0.5) + 
#   theme(legend.position = "none",
#         panel.background = element_rect(fill = NA, color = "gray"),
#         panel.grid = element_blank(),
#         axis.text = element_blank(),
#         axis.title = element_blank(),
#         axis.ticks = element_blank(),
#         strip.text = element_text(size = 18))
# mapCor
