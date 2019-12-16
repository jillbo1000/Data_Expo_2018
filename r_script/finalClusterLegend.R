# Create a plot dedicated to creating a cluster legend. 
sqRow <- c(1, 2, 2, 1, 1)
sqRow <- c(sqRow, sqRow+1, sqRow+2, sqRow+3, sqRow+4, sqRow+5)
sqCol <- rep(c(1, 1, 2, 2, 1), 6)
id = c(1, 1, 1, 1, 1)
id <- c(id, id+1, id+2, id+3, id+4, id+5)

labels <- c("Cali-Florida", "Southwest", "Midwest","Southeast", "Northeast", "Intermountain-West")

# Manually define the color scheme
set1 <- c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#f781bf")
#set1 <- set1[c(4, 2, 3, 6, 1, 5)] # Reorder to match the original layout
#labels <- labels[c(4, 2, 3, 6, 1, 5)] 

squareDF <- data.frame(Col = sqCol,
                       Row = sqRow,
                       id = id)

tleg <- ggplot(squareDF) + 
  geom_polygon(aes(x = Col, y = Row, group = id), fill = set1[id]) + 
  coord_fixed() +
  xlim(0, 5) + 
  annotate("text", x = 3.5, y = 6:1 + 0.5, label = labels, size = 6) + 
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.grid = element_blank())


tleg

pdf("../images/clusterLabels.pdf", height = 7, width = 4)
tleg
dev.off()
