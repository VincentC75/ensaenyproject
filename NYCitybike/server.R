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

load("../data/alldata.Rda")

shinyServer(function(input, output) {

  output$carte <- renderLeaflet({
    mlat  <- mean(alldata$station.latitude)
    mlong <- mean(alldata$station.longitude)
    
    
    #ColorPal <- colorNumeric(scales::seq_gradient_pal(low = "red", high = "green"),
    #                         domain = st$thresholded_available_bikes)
    m <- leaflet(data = alldata) %>%
      addTiles() %>%
      addCircles(~ station.longitude, ~ station.latitude, popup = ~ sprintf("<b> Mean Speed: %s</b>",as.character(meanspeed_in)),
                 radius = ~ 0.5*sqrt(trips_out),
#                 color = ~ ColorPal(thresholded_available_bikes),
                 stroke = TRUE, fillOpacity = 0.75) %>%
#      addCircles(data = geo(), ~ lon, ~ lat, color = "black", radius = 20) %>%
      setView(lng = mlong, lat = mlat, zoom = input$zoom)
    
    m
  })
  

})
