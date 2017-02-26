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

mycolors <- c("red", "blue", "green")

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
                       box(
                           title = "Select Top",
                           status = "primary",
                           width = 12,
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           sliderInput("top", "", ticks = FALSE, min = 1, max = nrow(dataj), value = nrow(dataj))),
                       box(
                           title = "Cluster",
                           status = "primary",
                           width = 12,
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           checkboxGroupInput(inputId = "Cluster",
                                              label = "",
                                              choices = c("Cluster 1" = 1,"Cluster 2" =2 ,"Cluster 3" =3),
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
                                            choices = c("Age moyen (Boxplot)" = 1,
                                                        "Age moyen (Barplot)" =2,
                                                        "Hommes / Femmes (Nb trajets)" = 3,
                                                        "Hommes / Femmes (%)" = 4,
                                                        "Vitesse moyenne" = 5,
                                                        "Distance moyenne" = 6),
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
      amBoxplot(  ((trips_out * mean_age_out + trips_in * mean_age_in)/(trips_out+trips_in)) ~ clust, data= dataj, col = mycolors, main = "Age moyen")
    } else if (input$Compare == 2) {
      amBarplot(x = "clust", y = "mean_age", data = statcluster, labelRotation = -45, main = "Age Moyen", show_values = TRUE) 
    } else if (input$Compare == 3) {
      amBarplot(x = "clust", y = c("trips_men", "trips_women"), data = statcluster, stack_type = "regular", groups_color = c("#87cefa", "pink"))    
    } else if (input$Compare == 4) {
      amBarplot(x = "clust", y = c("percent_men", "percent_women"), data = statcluster, stack_type = "regular", groups_color = c("#87cefa", "pink"))    
    } else if (input$Compare == 5) {
      amBoxplot( ((trips_out * meanspeed_out + trips_in * meanspeed_in)/(trips_out+trips_in)) ~ clust, data= dataj, col = mycolors, main = "Vitesse moyenne")
    } else if (input$Compare == 6) {
      amBoxplot( ((trips_out * meandist_out + trips_in * meandist_in)/(trips_out+trips_in)) ~ clust, data= dataj, col = mycolors, main = "Distance moyenne")
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
    #data_disp <- dataj[dataj$clust %in% input$Cluster,]
    data_disp <- dataj %>%
      filter(clust %in% input$Cluster) %>%
      mutate(trips_total = trips_in+trips_out) %>%
      arrange(desc(trips_total)) %>%
      filter(row_number() <= input$top)

    mlat  <- mean(dataj$station.latitude)
    mlong <- mean(dataj$station.longitude)
    ColorPal <- colorFactor(mycolors, dataj$clust)
    
    leafletProxy("carte", data = data_disp) %>%
      clearShapes() %>%
      addCircles(~ station.longitude, ~ station.latitude,
                 popup = ~ sprintf("<b>%s</b><br>Mean Speed: %s km/h<br>Mean Duration: %sm %ss<br>Mean Distance: %s km<br>%s%% men, %s%% women<br>%s trips (%s in, %s out)",
                                   station.name,
                                   as.character(round(((trips_in*meanspeed_in+trips_out*meanspeed_out)/(trips_in+trips_out)),2)),
                                   as.character(round(((trips_in*meanduration_in+trips_out*meanduration_out)/(trips_in+trips_out))/60,0)),
                                   as.character(round(((trips_in*meanduration_in+trips_out*meanduration_out)/(trips_in+trips_out))%%60,0)),
                                   as.character(round(((trips_in*meandist_in+trips_out*meandist_out)/(trips_in+trips_out)),2)),
                                   as.character(round((trips_in*percent_male_in+trips_out*percent_male_out)/(trips_in+trips_out),2)),
                                   as.character(round(100 - (trips_in*percent_male_in+trips_out*percent_male_out)/(trips_in+trips_out),2)),
                                   as.character(trips_in+trips_out),
                                   as.character(trips_in),
                                   as.character(trips_out)
                 ),
                 radius = ~ 0.8*sqrt(trips_out + trips_in),
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
