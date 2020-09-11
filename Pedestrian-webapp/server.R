library(shiny)

shinyServer(function(input, output) {
    
    # User filter the sensor ID
    filtered <- reactive({
        ped.data %>% 
            filter(sensorID == input$id)
    })
    
    # Plot time series
    output$figTime <- renderPlot({
        ggplot(filtered(), aes(x=datetime, y=count)) +
            geom_line() +
            theme_minimal()
    })
    
    # Sensors location on map
    output$mapSensors <- renderLeaflet({
        leaflet(ped.summary) %>% 
            #addAwesomeMarkers(lng=~long, lat=~lat, label=~sensorID) %>% 
            addCircleMarkers(lng=~long, lat=~lat, radius=~avg/100,
                             fillOpacity = 0.2, label=~sensorID,
                             color=~ifelse(sensorID==input$id, "red", "blue")) %>% 
            fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat)) %>% 
            addProviderTiles(providers$CartoDB.Positron)
    })
    

})
