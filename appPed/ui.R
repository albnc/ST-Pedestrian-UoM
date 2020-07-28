library(shiny)

# Define UI for application that draws a histogram
shinyUI(
    # Page layout
    fluidPage(

        # Application title
        titlePanel("Spatio-Temporal Pedestrian Analysis"),
        # Application Author
        tags$p("André Luiz Cunha"),
        tags$hr(),
        
        mainPanel(plotOutput("figTime"))
    )
)
