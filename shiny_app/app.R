library(GGally)
library(reshape2)
library(lubridate)
library(tidyr)
library(cluster)
library(fpc)
library(ggmap)
library(gridExtra)
library(grid)
library(dplyr)
library(shiny)
library(rgl)
library(maps)
library(mapdata)
library(RColorBrewer)
library(ggplot2)

# The package fiftystater was retired on CRAN shortly after the Data Expo. 
# The tarbell is included in the repository and can be insalled with the 
# following code from home directory of this repository: 
# install.packages("fiftystater_1.0.1.tar.gz", repos = NULL, type = "source")
library(fiftystater) # this package has been retired on CRAN, but the tarbell is included in the repository

library(maps)
library(shinythemes)
library(shinydashboard)

#------------------------------------------------------------------------------
# Data for the PCP App
#------------------------------------------------------------------------------

cluster2 <- read.csv("data/clusterMapFinal.csv")
mapdata <- read.csv("data/locationsFinal.csv")
mapdata$ClustName <- as.character(mapdata$ClustName)
mapdata$cluster <- rep(0, nrow(mapdata))


longitude <- c(-117.25, -108.50)
latitude <- c(27.2, 27)

mapdata$latitude[mapdata$state == "Alaska"] <- latitude[1]
mapdata$latitude[mapdata$state == "Hawaii"] <- latitude[2]
mapdata$longitude[mapdata$state == "Alaska"] <- longitude[1]
mapdata$longitude[mapdata$state == "Hawaii"] <- longitude[2]

mapdata$state <- tolower(mapdata$state)


cluster2$name <- paste(cluster2$city, cluster2$stateABB, sep = ", ")

clusterSum <- cluster2 %>% group_by(Cluster) %>% summarize(maxabsG = mean(mxT_mean_abs))
cluster2 <- left_join(cluster2, clusterSum, by = "Cluster") %>% arrange(maxabsG, mxT_mean_abs)
cluster2$maxabsG <- NULL

cluster2$AirPtCd <- factor(cluster2$AirPtCd, levels = cluster2$AirPtCd)
cluster2$Cluster <- factor(cluster2$Cluster, levels = unique(cluster2$Cluster))

cluster2 <- cluster2 %>% 
  dplyr::select(-mxT_sd, -mnT_sd, -mxT_sd_abs, -mnT_sd_abs,
                -mxT_mean, -mnT_mean, -mxT_mean_abs, -mnT_mean_abs,
                -BSS)

pal2 <- brewer.pal(8, "Set1")[c(1:5, 8)]
pal2 <- pal2[rev(c(1, 4, 2, 3, 6, 5))]

cluster2.sd <- cluster2 %>% 
  dplyr::select(contains("Sd"), elevation, dist2coast, Cluster, AirPtCd)
cluster2.sd[, 1:15] <- scale(cluster2.sd[, 1:15])

cluster2.other <- cluster2 %>% 
  dplyr::select(-contains("Sd"), -city, -state, -longitude, 
                -latitude, -stateABB, -name)
cluster2.other[, 2:16] <- scale(cluster2.other[, 2:16])

cluster2.sd.long <- melt(cluster2.sd, id.vars = c("Cluster", "AirPtCd"))
cluster2.sd.long$value[is.element(cluster2.sd.long$variable, c("elevation", "dist2coast"))] <- 0
cluster2.other.long <- melt(cluster2.other, id.vars = c("Cluster", "AirPtCd"))


#------------------------------------------------------------------------------
# Data for the Scatterplot App
#------------------------------------------------------------------------------

clust <- read.csv("data/locationsFinal.csv")
weather1 <- read.csv("data/summary_lag_shiny.csv")
weather1 <- left_join(weather1, clust[, c(6, 18:21)])
weather1 <- weather1[, c(36, 34, 37, 1:33, 35, 38:47)]
weather1$BSS <- 1 - weather1$BSS

longitude <- c(-117.25, -108.50)
latitude <- c(27.2, 27)

weather1$latitude[weather1$state == "Alaska"] <- latitude[1]
weather1$latitude[weather1$state == "Hawaii"] <- latitude[2]
weather1$longitude[weather1$state == "Alaska"] <- longitude[1]
weather1$longitude[weather1$state == "Hawaii"] <- longitude[2]

weather1$state <- tolower(weather1$state)

weather1 <- tbl_df(weather1)

weather1 <- weather1[, c(46:47, 39:41, 1:3, 5, 45)]
colnames(weather1)[c(1:2, 6:8)] <- c("Station", "Cluster", "MinTempError", "MaxTempError", "BSS")
weather1[, c(6:8)] <- round(weather1[, c(6:8)], 3)

weather1 <- select(weather1, Station, Cluster, MinTempError, MaxTempError,
                    BSS, latitude, longitude, color, state, lag)


ui <- dashboardPage(skin = "purple", 
                    
  dashboardHeader(title = "Data Expo 2018"),
  dashboardSidebar(sidebarMenu(
    menuItem("Region Features", tabName = "pcpTab"),
    menuItem("Scatterplots", tabName = "scatTab")
  )),

  dashboardBody(
    
    tabItems(
      
      #------------------------------------------------------------------------------
      # UI for PCP 
      #------------------------------------------------------------------------------
      
      tabItem(tabName = "pcpTab",
              h2("Parallel Coordinate Plot of Weather Features by Region"), 
              h5("Select a region to highlight it on the parallel coordinate plot."), 
              
              
              fluidRow(box(width = NULL, status = "primary",
                           radioButtons("region", 
                                    h4("Weather Region"), 
                                    choiceNames = list(HTML("<font color='#f781bf'>Cali-Florida</font>"), 
                                                   tags$span(style = "color:#4daf4a", "Southeast"), 
                                                   tags$span(style = "color:#377eb8", "Northeast"), 
                                                   tags$span(style = "color:#e41a1c", "Intermountain West"), 
                                                   tags$span(style = "color:#984ea3", "Midwest"), 
                                                   tags$span(style = "color:#ff7f00", "Southwest")), 
                                    choiceValues = c(4, 3, 2, 5, 1, 6),
                                    selected = 4, inline = TRUE))
              ),
              
              fluidRow(
                column(5, box(width = NULL, status = "primary",(plotOutput("mapPCP")))), 
                
                column(7, box(width = NULL, status = "primary",(plotOutput("pcp"))))
                
              )
              
              
              
      ),
      
      
      #------------------------------------------------------------------------------
      # UI for the Scatterplot App
      #------------------------------------------------------------------------------
      
      tabItem(tabName = "scatTab",
              h2("Forecast Error Scatterplot"),
              h5("Click or brush points in the scatterplot to identify weather stations. 
                 Click on a point to select it and click on it again to deselect it. 
                 Double-clicking on the scatterplot will clear all highlighed points."), 
              
              fluidRow(
        
      ), 
      
      column(2,  
             box(width = NULL, status = "primary", 
                 radioButtons("xvar", 
                              h4(strong("X-Variable")), 
                              choices = list("Minimum Temperature" = "MinTempError", 
                                             "Maximum Temperature" = "MaxTempError", 
                                             "Precipitation" = "BSS"), 
                              selected = "MinTempError"), 
                 
                 radioButtons("yvar", 
                              h4(strong("Y-Variable")), 
                              choices = list("Minimum Temperature" = "MinTempError", 
                                             "Maximum Temperature" = "MaxTempError", 
                                             "Precipitation" = "BSS"), 
                              selected = "MaxTempError"), 
                 
                 radioButtons("lag", 
                              h4(strong("Days from Forecast")), 
                              choices = list("Over All Forecasted Days" = 10,
                                             "Same Day" = 0, 
                                             "1 Day Before" = 1, 
                                             "2 Days Before" = 2, 
                                             "3 Days Before" = 3,
                                             "4 Days Before" = 4,
                                             "5 Days Before" = 5), 
                              selected = 10), 
                 
                 radioButtons("rs",
                              h4(strong("Axis Options")), 
                              choices = list("Local Scale" = 0,
                                             "Global Scale" = 1),
                              selected = 0))), 
      
      
      column(10,
             
             fluidRow(column(5, 
                             box(width = NULL, status = "primary", 
                                 plotOutput("scatter", click = "click_points", 
                                            dblclick = "dblClear", brush = "plot_brush"))), 
                      
                      column(7,
                             box(width = NULL, status = "primary", plotOutput("map")))),
             
             box(width = NULL, status = "primary", column(10, dataTableOutput("table"))
             )
      ))
      
  
  
  )))
             
             
server <- function(input, output) {
  
  #------------------------------------------------------------------------------
  # Define reactive datasets for scatter app
  #------------------------------------------------------------------------------

  values <- reactiveValues()
  values$DT <- data.frame(Station = factor(),
                          Cluster = factor(),
                          MinTempError = numeric(),
                          MaxTempError = numeric(),
                          BSS = numeric(),
                          latitude = numeric(),
                          longitude = numeric(),
                          color = factor(), 
                          state = factor(), 
                          lag = numeric()
  )
  
  weather <- reactiveValues()
  weather$DT <- data.frame(Station = factor(),
                           Cluster = factor(),
                           MinTempError = numeric(),
                           MaxTempError = numeric(),
                           BSS = numeric(),
                           latitude = numeric(),
                           longitude = numeric(),
                           color = factor(), 
                           state = factor(), 
                           lag = numeric()
  )
  
  
  #------------------------------------------------------------------------------
  # Server functions for PCP
  #------------------------------------------------------------------------------
     
    output$mapPCP <- renderPlot({
      
      tmp <- mapdata[mapdata$Cluster6 == as.numeric(input$region), ]
      
      p <- ggplot(mapdata, aes(map_id = state)) +
        # map points to the fifty_states shape data
        geom_map(fill = "gray90", color = "gray80", map = fifty_states) +
        expand_limits(x = fifty_states$long, y = fifty_states$lat) +
        coord_map() +
        scale_x_continuous(breaks = NULL) +
        scale_y_continuous(breaks = NULL) +
        labs(x = "", y = "") +
        theme(legend.position = "bottom",
              panel.background = element_blank())
      
      p +   
        geom_point(aes(x = longitude, y = latitude), color = mapdata$color, size = 2, alpha = 0.5) +
        # geom_point(data = tmp, aes(x = longitude, y = latitude),
        #            fill = tmp$color, size = 5, shape = 21) +
        geom_point(data = tmp, aes(x = longitude, y = latitude),
                   fill = tmp$color, size = 4, shape = 21)
      
    })
    
    output$pcp = renderPlot({
      
      cluster2.sd.long.sub <- cluster2.sd.long[is.element(as.character(cluster2.sd.long$Cluster), 
                                                          input$region), ]
      cluster2.other.long.sub <- cluster2.other.long[is.element(as.character(cluster2.other.long$Cluster), 
                                                                input$region), ]
      
      suby <- cluster2.sd.long.sub$value[cluster2.sd.long.sub$variable == "Sd_Max_Temp"]
      suby[3] <- suby[3] + 0.2
      suby[5] <- suby[5] - 0.2
      cluster2.sd.long$title1 <- "Measurement Standard Deviations"
      # Angle label help:
      # - https://stackoverflow.com/questions/1330989/rotating-and-spacing-axis-labels-in-ggplot2 
      p1 <- ggplot(cluster2.sd.long, aes(x = variable, y = value, group = factor(AirPtCd))) + 
        geom_path(aes(color = factor(Cluster)), alpha = 0.3) +
        scale_color_manual(values = pal2) + 
        geom_path(data = cluster2.sd.long.sub,
                  aes(x = variable, y = value, group = factor(AirPtCd), color = factor(Cluster)), 
                  lwd = 1, alpha = 0.7) +
        # annotate("text", x = 0, y = 0.5,
        #          label = "Standard Deviation") +
        # expand_limits(x = -.9) + 
        facet_grid(~title1) + 
        # expand_limits(x = c(0, 16)) + 
        scale_x_discrete(labels=c("Max Temp", "Min Temp", "Precipitation", "Max Dew Point", 
                                  "Min Dew Point", "Max Humidity", "Min Humidity", 
                                  "Max Sea Level Pressure", "Min Sea Level Pressure", 
                                  "Visibility", "Cloud Cover", "Mean Wind Speed", 
                                  "Max Wind Speed", "Elevation", 
                                  "Distance to Coast")) + 
        theme_bw() +
        theme(legend.position = "none",
              axis.text.x = element_text(angle = 60, vjust  = 1, hjust = 1, size = 13),
              axis.title = element_blank(), 
              strip.text.x = element_text(size = 16))
      
      # Expand idea from scale_x_discrete help file. 
      cluster2.other.long$title1 <- "Measurement Means"
      p2 <- ggplot(cluster2.other.long, aes(x = variable, y = value, group = factor(AirPtCd))) + 
        geom_path(aes(color = factor(Cluster)), alpha = 0.3) +
        scale_color_manual(values = pal2) + 
        geom_path(data = cluster2.other.long.sub,
                  aes(x = variable, y = value, group = factor(AirPtCd), color = factor(Cluster)), 
                  lwd = 1, alpha = 0.7) + 
        # annotate("text", x = 0, y = -0.5,
        #          label = "Mean") +
        # expand_limits(x = -.9) + 
        # expand_limits(x = c(0, 16)) + 
        facet_grid(~title1) + 
        theme_bw() +
        theme(legend.position = "none",
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              axis.title = element_blank(), 
              strip.text.x = element_text(size = 16))
      
      grid.draw(rbind(ggplotGrob(p2), ggplotGrob(p1), size = "last"))
      
      
      
    }) 
    
    
    #------------------------------------------------------------------------------
    # Server functions for Scatter
    #------------------------------------------------------------------------------

    output$scatter <- renderPlot({
      
      tmp <- brushedPoints(weather$DT, input$plot_brush)
      
      # isolate(print(values$DT))
      # isolate(print("plot"))
      
      if(input$xvar == "MinTempError") {
        xl <- "Minimum Temperature Error (F)"
        xlm <- c(1.7, 8.2)
        xb <- c(2:8)
      } else if(input$xvar == "MaxTempError") {
        xl <- "Maximum Temperature Error (F)"
        xlm <- c(1, 7)
        xb <- c(1:7)
      } else {
        xl <- "Precipitation (1-BSS)"
        xlm <- c(0.4, 2.7)
        xb <- c(seq(0.5, 2.5, 0.5))
      }
      
      if(input$yvar == "MinTempError") {
        yl <- "Minimum Temperature Error (F)"
        ylm <- c(1.7, 8.2)
        yb <- c(2:8)
      } else if(input$yvar == "MaxTempError") {
        yl <- "Maximum Temperature Error (F)"
        ylm <- c(1, 7)
        yb <- c(1:7)
      } else {
        yl <- "Precipitation (1-BSS)"
        ylm <- c(0.4, 2.7)
        yb <- c(seq(0.5, 2.5, 0.5))
      }
      
      
      q <- ggplot(weather$DT, aes_string(x = input$xvar, y = input$yvar)) +
        geom_point(color = weather$DT$color, size = 2, alpha = 0.4) +
        geom_point(data = values$DT, fill = values$DT$color, shape = 21, size = 4) +
        geom_point(data = tmp, fill = tmp$color, shape = 21, size = 4) +
        scale_x_continuous(breaks = xb) +
        scale_y_continuous(breaks = yb) +
        labs(x = xl, y = yl) +
        theme_bw()
      
      if(input$rs == "1") {
        q <- q + scale_x_continuous(breaks = xb, limits = xlm) +
          scale_y_continuous(breaks = yb, limits = ylm) 
      }
      
      q
      
    })
    
    output$map <- renderPlot({
      
      tmp <- brushedPoints(weather$DT, input$plot_brush)
      tmp2 <- brushedPoints(weather$DT, input$bp2)
      
      p <- ggplot(weather$DT, aes(map_id = state)) +
        # map points to the fifty_states shape data
        geom_map(fill = "gray90", color = "gray80", map = fifty_states) +
        expand_limits(x = fifty_states$long, y = fifty_states$lat) +
        coord_map() +
        scale_x_continuous(breaks = NULL) +
        scale_y_continuous(breaks = NULL) +
        labs(x = "", y = "") +
        theme(legend.position = "bottom",
              panel.background = element_blank()) 
      
      p +   
        geom_point(aes(x = longitude, y = latitude), color = weather$DT$color, size = 2, alpha = 0.5) +
        geom_point(data = values$DT, aes(x = longitude, y = latitude),
                   fill = values$DT$color, size = 5, shape = 21) +
        geom_point(data = tmp, aes(x = longitude, y = latitude),
                   fill = tmp$color, size = 5, shape = 21) 
      
    }) 
    
  observeEvent(input$click_points, {
    # each input is a factor so levels are consistent for plotting characteristics
    tmp <- nearPoints(weather$DT, input$click_points, addDist = TRUE)
    
    add_row <- data.frame(Station = tmp$Station,
                          Cluster = tmp$Cluster,
                          MinTempError = tmp$MinTempError,
                          MaxTempError = tmp$MaxTempError,
                          BSS = tmp$BSS,
                          latitude = tmp$latitude,
                          longitude = tmp$longitude,
                          color = tmp$color, 
                          state = tmp$state, 
                          lag = tmp$lag
    )
    
    # add row to the data.frame
    values$DT <- rbind(add_row, values$DT)
    dup <- duplicated(values$DT) + duplicated(values$DT, fromLast = TRUE) 
    values$DT <- values$DT[!dup, ]
    
    return(values$DT)
    
  })
  
  observeEvent(input$lag, {
    # each input is a factor so levels are consistent for plotting characteristics
    
    weather$DT <- weather1[weather1$lag == as.numeric(input$lag), ]
    isolate(print("weather - DAT"))
    isolate(print(head(weather$DT)))
    isolate(print("Reactive - DAT"))
    isolate(print(values$DT))
    
    apts <- as.character(values$DT$Station)
    # isolate(apts)
    values$DT <- values$DT[values$DT$BSS == 1000, ]
    values$DT <- weather$DT[is.element(as.character(weather$DT$Station), apts), ]
    
    isolate(print("Reactive - DAT 2"))
    isolate(print(values$DT))
    # isolate(print(weather$DT))
    # isolate(print(input$lag))
    # isolate(print(class(input$lag)))
    
    return(weather$DT)
    
  })
  
  observeEvent(input$dblClear, {
    # each input is a factor so levels are consistent for plotting characteristics
    if(!is.null(input$dblClear)) values$DT <- values$DT[values$DT$BSS == 1000, ]

    return(values$DT)
    
  })
  
  output$table <- renderDataTable({
    
    isolate(values$DT)
    tmp <- brushedPoints(weather$DT, input$plot_brush)
    tmp <- select(tmp, Station, Cluster, MinTempError, MaxTempError, BSS)
    # colnames(tmp[, c(1:2, 6:8)]) <- colnames(values$DT)[1:5]
    
    tmp2 <- select(values$DT, Station, Cluster, MinTempError, MaxTempError, BSS)
    
    tab <- rbind(tmp2, tmp)
    colnames(tab)[2:5] <- c("Region", "Min Temp Error", "Max Temp Error", "1-BSS")
    
    tab[!duplicated(tab), ]
    
  })
  
}

shinyApp(ui, server)





