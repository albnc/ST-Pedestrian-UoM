library(shiny)

# Define UI for application that draws a histogram
shinyUI(
    # Page layout
    #fluidPage(
    pageWithSidebar(

        # Application title
        #titlePanel("Spatio-Temporal Pedestrian Analysis"),
        headerPanel("Spatio-temporal Pedestrian Analysis"),
        sidebarPanel(
            
            # Application Author
            tags$p("André Luiz Cunha"),
            tags$hr(),
            
            # Input
            numericInput("id", "Sensor ID", 
                        value = 57, min = pedata$sensorID[[1]],
                        max = pedata$sensorID[[length(pedata$sensorID)]])
            ),
            
        
        mainPanel(
            plotOutput("figTime")
            )
    )
)
