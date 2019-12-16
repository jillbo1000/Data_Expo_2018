# This is a illustrative plot of the process used to create the ellipses 
# in the ellipse plot. 
# Date: 3-14-2019
library(ggplot2)

# First create a temporary data frame of an ellipse with semi-major axis y = x. 
# Assume rho = 0.5
theta = seq(0, 2*pi, .001)
rho = 0.5
r = (1-rho)^2 / (1 - sqrt(rho*(2-rho))*cos(theta - pi/4))

tempdf <- data.frame(x = r*cos(theta), y = r*sin(theta))


pdf("../images/final/paper/sampleEllipse.pdf", width = 4, height = 4)
ggplot(tempdf) +  
  # Define the axes
  geom_hline(yintercept = 0, color = "gray") +
  geom_vline(xintercept = 0, color = "gray") +
  scale_x_continuous(breaks = c(0)) + 
  scale_y_continuous(breaks = c(0)) + 
  # Define the ellipse
  geom_path(aes(x = x, y = y),lwd = 1) + 
  # Define the semi-minor axis
  geom_abline(slope = 1, intercept = 0, color = "gray", lty = 2) + 
  geom_segment(x = 0, y = 0, xend = tempdf$x[1000], yend = tempdf$y[1000]) +
  geom_curve(x = .5, y = 0, xend = .5*cos(theta[1000]), yend = .5*sin(theta[1000]),
             arrow = arrow(length = unit(0.03, "npc"))) + 
  # Label points
  annotate("point", x = 0, y = 0, size = 3) +
  annotate("point", x = sqrt(rho*(2-rho)*2), y = sqrt(rho*(2-rho)*2), size = 3) +
  annotate("text", x = 0, y = 0, label = "F1", hjust = 1, vjust = -1, size = 5) +
  annotate("text", x = sqrt(rho*(2-rho)*2), y = sqrt(rho*(2-rho)*2), label = "F2", 
           hjust = 1, vjust = -1, size = 5) +
  annotate("text", x = tempdf$x[1000]/2, y = tempdf$y[1000]/2, label = "r",
           hjust = 1, vjust = -1, size = 5) + 
  annotate("text", x = .5*cos(theta[500]), y = .5*sin(theta[500]), 
           label = latex2exp::TeX("$\\theta$", output = "expression"), 
           hjust = 1, vjust = 1, size = 5) +
  theme_bw() + 
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_blank()) + 
  coord_equal()
dev.off()
