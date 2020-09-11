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
        ),
        mainPanel(
             tabsetPanel(
                 tabPanel(
                     "Data",
                     leafletOutput("mapSensors", width = "100%"),
                     plotOutput("figTime", width="100%")
                 ),
                 
                 tabPanel(
                     "Time Analysis",
                     "Something here"
                 ),
                 
                 tabPanel(
                     "Spatial Analysis",
                     "Another thing"
                 )
             )
        )
     )
))
