library(shiny)
library(dygraphs)

# Define server logic
shinyServer(function(input, output) {
    output$figTime <- renderPlot({
      ggplot(sy, aes(x=datetime, y=count)) + geom_line()
      # p <- dygraph(don) %>%
      #   dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors="#D8AE5A") %>%
      #   dyRangeSelector() %>%
      #   dyCrosshair(direction = "vertical") %>%
      #   dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
      #   dyRoller(rollPeriod = 1)
      
    })

})
