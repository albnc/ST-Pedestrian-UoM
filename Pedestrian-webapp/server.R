library(shiny)

shinyServer(function(input, output) {


# Data processing ----------------------------------------------------------
    # User filter the sensor ID
    filtered <- reactive({
        ped.data %>% 
            filter(sensorID == input$id)
    })
    
    # Wavelet coefficients
    wav.coefs <- reactive({
        wav.func(filtered())
    })
    
    
# TIME SERIES -------------------------------------------------------------
    # Plot with GGPLOT2
    output$ggplotFig <- renderPlot({
        ggplot(filtered(), aes(x=datetime, y=count)) +
            geom_line() +
            xlab("Datetime") + ylab("Number of pedestrian [ped/h]") +
            theme_minimal()      
        })
        
    # Plot with PLOTLY
    output$plotlyFig <- renderPlotly({
        ggplot(filtered(), aes(x=datetime, y=count)) +
            geom_line() +
            xlab("Datetime") + ylab("Number of pedestrian [ped/h]") +
            theme_minimal()
    })
    # Plot with DYGRAPHS
    output$dygraphFig <- renderDygraph({
        ped.xts <- filtered() %>% 
            select(datetime, count)
        ped.xts <- xts(x=ped.xts$count, order.by = ped.xts$datetime) 
        dygraph(ped.xts) %>% 
            dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors="#D8AE5A") %>%
            dyRangeSelector() %>%
            dyCrosshair(direction = "vertical") %>%
            dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
            dyRoller(rollPeriod = 1)
        
    })
    
    ## Plot weekly traffic
    output$weekFig <- renderPlot({
        weekorder <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
        ggplot(filtered(), aes(x=hour(datetime), y=count, group=date(datetime))) +
            geom_line() +
            facet_wrap(~factor(weekdays(datetime, TRUE), weekorder)) +
            theme_minimal()
    })

# MAP ---------------------------------------------------------------------
    # Sensors location on map
    output$mapSensors <- renderLeaflet({
        leaflet(ped.summary) %>% 
            #addAwesomeMarkers(lng=~long, lat=~lat, label=~sensorID) %>% 
            addCircleMarkers(lng=~long, lat=~lat, radius=~sqrt(avg)/2,
                             fillOpacity = 0.2, label=~content,
                             color=~ifelse(sensorID==input$id, "red", "blue")) %>% 
            fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat)) %>% 
            addProviderTiles(providers$CartoDB.Positron)
    })
    

# TIME ANALYSIS -----------------------------------------------------------
    # Raw data heatmap plot
    output$dataTS <- renderPlot({
        ggplot(filtered(), aes(x=day(datetime),y=hour(datetime),fill=count)) +
            geom_tile(color="white", size=0.1) +
            scale_fill_viridis(name="Hrly Count" ,option ="C") +
            facet_wrap(~month(datetime, label = TRUE)) +
            scale_y_continuous(trans = "reverse", breaks = seq(0,23,2)) +
            scale_x_continuous(breaks =c(1,7,14,21,28)) +
            xlab("Day of month") + ylab("Hour of day")
    })
    
    # Raw data outliers
    output$outlierTS <- renderPlotly({
        thresh.min <- mean(filtered()$count) - input$std * sd(filtered()$count)
        thresh.max <- mean(filtered()$count) + input$std * sd(filtered()$count)
        
        ggplot(filtered(), aes(x=datetime, y=count)) +
            geom_line() +
            geom_point(data = filter(filtered(), count > thresh.max | count < thresh.min),
                       aes(x=datetime, y=count), color="red") +
            xlab("Datetime") + ylab("Number of pedestrian [ped/h]") +
            theme_minimal()
    })
    
    # Wavelet heatmap plot
    output$heatWavelet <- renderPlot({
        ggplot(wav.coefs(), aes(x=day(datetime),y=hour(datetime),fill=get(input$WaveSignal))) +
            geom_tile(color="white", size=0.1) +
            scale_fill_viridis(name=paste0("Hrly ", input$WaveSignal),option ="C") +
            facet_wrap(~month(datetime, label = TRUE)) +
            scale_y_continuous(trans = "reverse", breaks = seq(0,23,2)) +
            scale_x_continuous(breaks =c(1,7,14,21,28)) +
            xlab("Day of month") + ylab("Hour of day")
    })
    
    # Wavelet coefficients outliers
    output$anomaliesWavelet <- renderPlotly({
        values <- wav.coefs() %>% select(input$WaveSignal)
        thresh.min <- mean(values) - input$std * sd(values)
        thresh.max <- mean(values) + input$std * sd(values)
        
        ggplot(wav.coefs(), aes(x=datetime, y=input$WaveSignal)) +
            geom_line() +
            geom_point(data = filter(wav.coefs(), input$WaveSignal > thresh.max | input$WaveSignal < thresh.min),
                       aes(x=datetime, y=input$WaveSignal), color="red") +
            xlab("Datetime") +
            theme_minimal()
    })
})
