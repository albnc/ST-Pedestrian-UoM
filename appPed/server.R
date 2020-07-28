library(shiny)

# Define server logic
shinyServer(function(input, output) {
    output$figTime <- renderPlot({
        ggplot(sy, aes(x=datetime, y=count)) + geom_line()
    })

})
