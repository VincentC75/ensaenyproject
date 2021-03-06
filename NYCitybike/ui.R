#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(shinydashboard)




# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("NY Citybike travel speed."),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       sliderInput("zoom",
                   "Zoom:",
                   min = 1,
                   max = 25,
                   value = 12)
    ),
    mainPanel(
      textOutput("texte"),
      leafletOutput("carte", height = 600)
    )
  )
))
