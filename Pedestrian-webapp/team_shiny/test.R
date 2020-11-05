library(shiny)

events

shinyApp(
  ui = fluidPage(
    selectInput("state", "Choose a state:",
                list(`East Coast` = list("NY"=1, "NJ"=2, "CT"=3),
                     `West Coast` = list("WA", "OR", "CA"),
                     `Midwest` = list("MN", "WI", "IA"))
    ),
    textOutput("result")
  ),
  server = function(input, output) {
    output$result <- renderText({
      paste("You chose", input$state)
    })
  }
)