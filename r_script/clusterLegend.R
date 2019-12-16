# Create a legend for use in all of our paper figures (as we can no longer rely on the poster derived legends.)
library(tidyverse)

# Legend Code
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
           color = "white", size = c(5.5, 5.5, 5.5, 3.5, 5.5, 5.5)) + 
  theme(legend.position = "none",
        panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())
#=============================================================================


       