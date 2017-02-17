# Dashboard Shiny
library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(rAmCharts)

# Chargement des donnees
setwd("C:/Users/Vincent/Dropbox/LECEPE/nyproject/ensaenyproject")
load("data/201609-alldatacluster.Rda")
load("data/201609-alldata.Rda")
load("data/201609-statcluster.Rda")
dataj <- left_join(alldatacluster, alldata)
table(dataj$clust)
rm(alldata, alldatacluster)

mycolors <- c("red", "blue", "green", "magenta", "orange")

# Interface
ui <- dashboardPage(
  dashboardHeader(title = "NYC Citybike"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Metrics", tabName = "metrics", icon = icon("bar-chart"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                column(width = 2, 
#                       box(
#                           title = "Zoom",
#                           status = "primary",
#                           width = 12,
#                           solidHeader = TRUE,
#                           collapsible = TRUE,
#                           sliderInput("zoom", "", min = 1, max = 18, value = 12)),
                       box(
                           title = "Cluster",
                           status = "primary",
                           width = 12,
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           checkboxGroupInput(inputId = "Cluster",
                                              label = "",
                                              choices = c("Cluster 1" = 1,"Cluster 2" =2 ,"Cluster 3" =3,"Cluster 4" =4,"Cluster 5"=5),
                                              selected = 1:5
                           ))
                ),
                column(width = 10, leafletOutput("carte", height = 600))
              )
      ),
      
      # Second tab content
      tabItem(tabName = "metrics",
              fluidRow(
                column(width = 3, 
                       box(
                         title = "Metric",
                         status = "primary",
                         width = 12,
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         selectInput(inputId = "Compare",
                                            label = "",
                                            choices = c("Age moyen des départs" = 1,"Age moyen des arrivées" =2, "Vitesse moyenne des départs" =3, "Vitesse moyenne des arrivées" = 4, "Age moyen" = 5, "Hommes / Femmes (Nb trajets)" = 6, "Hommes / Femmes (%)" = 7),
                                            selected = 1
                                    ))
                )
                ,
                column(width = 9, amChartsOutput(outputId = "amchart"))
      )
    )
    ) 
  )
)

# Serveur
server <- function(input, output) {

  output$amchart <- renderAmCharts({
    if (input$Compare == 1) {
      amBoxplot(mean_age_out ~ clust, data= dataj, col = mycolors, main = "Age moyen pour les départs")
    } else if (input$Compare == 2) {
      amBoxplot(mean_age_in ~ clust, data= dataj, col = mycolors, main = "Age moyen pour les arrivées")
    } else if (input$Compare == 3) {
      amBoxplot(meanspeed_out ~ clust, data= dataj, col = mycolors, main = "Vitesse moyenne pour les départs")
    } else if (input$Compare == 4) {
      amBoxplot(meanspeed_in ~ clust, data= dataj, col = mycolors, main = "Vitesse moyen pour les arrivées")
    } else if (input$Compare == 5) {
      amBarplot(x = "clust", y = "mean_age", data = statcluster, labelRotation = -45, main = "Age Moyen", show_values = TRUE) 
    } else if (input$Compare == 6) {
      amBarplot(x = "clust", y = c("trips_men", "trips_women"), data = statcluster, stack_type = "regular", groups_color = c("#87cefa", "pink"))    
    } else if (input$Compare == 7) {
      amBarplot(x = "clust", y = c("percent_men", "percent_women"), data = statcluster, stack_type = "regular", groups_color = c("#87cefa", "pink"))    
    }
  })
  
  output$carte <- renderLeaflet({
    mlat  <- mean(dataj$station.latitude)
    mlong <- mean(dataj$station.longitude)
    ColorPal <- colorFactor(mycolors, dataj$clust)
    
    m <- leaflet(data = dataj) %>%
      addTiles() %>%
      addCircles(~ station.longitude, ~ station.latitude,
                 popup = ~ sprintf("<b> Mean Speed: %s</b>",as.character(meanspeed_in)),
                 radius = ~ 0.5*sqrt(trips_out),
                 color = ~ ColorPal(clust),
                 stroke = TRUE, fillOpacity = 0.75)  %>%
      setView(lng = mlong, lat = mlat, zoom = 12)
    m
  })
  
  observe({
#    print(input$Cluster)
    if (!is.null(input$Cluster)) {
    data_disp <- dataj[dataj$clust %in% input$Cluster,]

    mlat  <- mean(dataj$station.latitude)
    mlong <- mean(dataj$station.longitude)
    ColorPal <- colorFactor(mycolors, dataj$clust)
    
    leafletProxy("carte", data = data_disp) %>%
      clearShapes() %>%
      addCircles(~ station.longitude, ~ station.latitude,
                 popup = ~ sprintf("<b> Mean Speed: %s</b>",as.character(meanspeed_in)),
                 radius = ~ 0.5*sqrt(trips_out),
                 color = ~ ColorPal(clust),
                 stroke = TRUE #,fillOpacity = 0.75
                 )  %>%
      setView(lng = mlong, lat = mlat, zoom = 12)
    } else {
      leafletProxy("carte") %>% clearShapes()
        }
  })
  
  
}

shinyApp(ui, server)
