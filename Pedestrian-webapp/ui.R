library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    titlePanel("Capstone Project"),
    tags$hr(),
    
    # Author name
    tags$h2("Pedestrian Analysis: Melbourne, VIC"),
     # Page with sidebar
     sidebarLayout(

         # Sidebar
         sidebarPanel(
             tags$p("This project aims to..."),
             tags$h5("Team:"),
             tags$ul(
                 tags$li("André Luiz Cunha"),
                 tags$li("Patricia Sauri"),
                 tags$li("Jessica Tong"),
                 tags$li("Hans Gao"),
                 tags$li("Marcus Rzanovski"),
                 tags$hr()
             ),

             # Inputs
             numericInput("id", "Choose a sensor:",
                          value = ped.data$sensorID[[1]],
                          min = ped.data$sensorID[[1]],
                          max = ped.data$sensorID[[length(ped.data$sensorID)]]),
             
             leafletOutput("mapSensors", width = "100%")
        ),
        mainPanel(
             tabsetPanel(
                 tabPanel(
                     "Data",
                     tags$h2("Data Exploratory Analysis"),
                     tags$h3("Times series"),
                     dygraphOutput("dygraphFig", width="100%"),
                     plotlyOutput("plotlyFig", width="100%"),
                     plotOutput("ggplotFig", width="100%"),
                     tags$h3("Weekdays pattern"),
                     plotOutput("weekFig", width="100%")
                     
                 ),
                 
                 tabPanel(
                     "Time Analysis",
                     tags$h2("Raw data"),
                     tags$h3("Heatmap"),
                     tags$p("Considering the raw data"),
                     plotOutput("dataTS", width="100%"),
                     tags$h3("Outliers"),
                     sliderInput("std", "Select the deviation: ", 0, 5, 1.5, 0.1),
                     plotlyOutput("outlierTS", width="100%"),
                     tags$h2("Wavelet"),
                     tags$p("As the raw data was sampled each 1h, level 1 represents 2h, level 2 = 4h, level 3 = 8h, and level 4 = 16h of data aggregation"),
                     selectInput("WaveSignal", "Choose the coeffiecient: ",
                                 choices = c("D1", "S1", "D2", "S2", "D3", "S3", "D4", "S4")),
                     tags$h3("Heatmap"),
                     plotOutput("heatWavelet", width="100%"),
                     tags$h3("Anomalies - Wavelet"),
                     plotOutput("anomaliesWavelet", width="100%")
                     
                 ),
                 
                 tabPanel(
                     "Spatial Analysis",
                     "Another thing"
                 )
             )
        )
     )
))
