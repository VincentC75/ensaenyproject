#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggmap)
library(dplyr)

setwd("C:/Users/Vincent/Dropbox/LECEPE/ensaenyproject/NYCitybike")
load("../data/alldatacluster2.Rda")
load("../data/alldata.Rda")
#dataj <- full_join(alldata, alldatacluster)
dataj <- left_join(alldatacluster, alldata)
table(dataj$clust)
#alldata$cluster <- dataj$clust

shinyServer(function(input, output) {

  output$carte <- renderLeaflet({
    mlat  <- mean(dataj$station.latitude)
    mlong <- mean(dataj$station.longitude)
#    ColorPal <- colorNumeric(scales::seq_gradient_pal(low = "red", high = "green"), domain = alldata$cluster)
    #ColorPal <- colorNumeric(scales::seq_gradient_pal(low = "red", high = "green"), domain = alldata$cluster)
    ColorPal <- colorFactor(topo.colors(8), dataj$clust)    
    
    #ColorPal <- colorNumeric(scales::seq_gradient_pal(low = "red", high = "green"),
    #                         domain = st$thresholded_available_bikes)
    m <- leaflet(data = dataj) %>%
      addTiles() %>%
      addCircles(~ station.longitude, ~ station.latitude, popup = ~ sprintf("<b> Mean Speed: %s</b>",as.character(meanspeed_in)),
                 radius = ~ 0.5*sqrt(trips_out),
                 #color = ~ rainbow(cluster),
                 color = ~ ColorPal(clust),
                 stroke = TRUE, fillOpacity = 0.75) %>%
#      addCircles(data = geo(), ~ lon, ~ lat, color = "black", radius = 20) %>%
      setView(lng = mlong, lat = mlat, zoom = input$zoom)
    
    m
  })
  

})
