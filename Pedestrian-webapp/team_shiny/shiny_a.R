# City of Melbourne Pedestrian Count Data Event Analysis
# Web application to view results
# Author: Jessica Tong
# ENGR90037/8 Engineering Capstone Project 2020
# The University of Melbourne

library(tidyverse)
library(lubridate)
library(sf)
library(shiny)
library(leaflet)

# Data ----
# Basemap Lines
map_streetsped <- readRDS("mapstreetsped.rds")
map_streets <- readRDS("mapstreets.rds")
map_rail <- readRDS("maprail.rds")

# Sensors Locations
sensorloc <- readRDS("sensorloc.rds")
sensor_sf <- st_as_sf(sensorloc, coords = c("Longitude", "Latitude"), crs=4326)

# Events List with Temporal Spans
### events <- readRDS("events2019.rds")
events <- readRDS("events2019_a.rds")

# Pedestrian Counts, Wavelet Transformed Data, Before-and-After
before_after <- readRDS("beforeafter.rds")

# All identified outliers (Levels 1 to 5) for events and sensor no.
outlier_sensors <- readRDS("outliersensors_a.rds")
###outlier_sensors <- readRDS("outliersensors.rds")

# Hour-by-hour identified outliers
outlier_hour <- readRDS("outlierhours_a.rds")
###outlier_hour <- readRDS("outlierhours.rds")


# Define UI ----
ui <- fluidPage(
  titlePanel(
    "Measuring the Temporal and Spatial Impacts of Short-term Events on Pedestrian Flows"
    ),
  sidebarLayout(
    sidebarPanel(
      h4("Events 2019"),
      helpText("Event-specific results: all events occured in and around Melbourne City in 2019"),
      selectInput("chosen_event",
                  label = "Choose Event",
                  choices = setNames(events$Event_ID,events$EventString),
                  selected = 194
                  ### choices = unique(events$EventString),
                  ### selected = "White Night Melbourne Night 3 - Aug 24 19:00 to Aug 25 02:00"
                  ),
      # uiOutput("chosen_date"),
      strong("Event Information"),
      textOutput("chosen_event_schedule"),
      br(),
      textOutput("chosen_event_time"),
      br(),
      textOutput("chosen_event_type"),
      br(),
      textOutput("chosen_event_recurring"),
      br(),
      textOutput("chosen_event_area"),
      br(),
      textOutput("chosen_event_attendance"),
      br(),
      strong("Comparison Plot Across Sensors"),
      plotOutput("plot_dailycomparison", width = "100%"),
      p("Before Event: Corresponding 4-week average (top)"),
      p("During Event Day(s): Hourly pedestrian counts during chosen event day(s) (bottom)")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Event Results",
                 h3("Location and Temporal Span of Event Effect"),
                 fluidRow(
                   splitLayout(cellWidths = c("40%", "60%"),
                               leafletOutput("plot_outliers", width = "100%"),
                               # plotOutput("plot_outliers", width = "100%"),
                               plotOutput("plot_temporal", width = "100%")
                   )
                 ),
                 p("Outliers were detected in pedestrian counting sensors using a multi-resolution wavelet analysis. The duration of event effect on the network was estimated from the outliers detected."),
                 # p("Outliers were detected in pedestrian counting sensors using a multi-resolution wavelet analysis."),
                 # h3("Temporal Span of Event Effect"),
                 # p("The duration of event effect on the network can be estimated from the level at which outliers across all sensors were detected in the pedestrian counting data."),
                 h3("Scale of Event Effect"),
                 fluidRow(
                   splitLayout(cellWidths = c("40%", "60%"), 
                               height = "500px", 
                               plotOutput("plot_cluster"),
                               plotOutput("plot_scale")
                 )),
                 p("Before-and-after comparison of pedestrian counts during event to 4-week historical average. Sensors within the same clusters experienced similar increases/decreases in pedestrian flows throughout the event's temporal span."),
                 radioButtons("ncluster",
                              label = "Choose no. of clusters for plots above",
                              choices = c(1:5),
                              selected = 3,
                              inline = TRUE
                              )
                 ),
        ## PANEL 2
        tabPanel("Pedestrian Sensor Information",
                 h3("About Melbourne City's Pedestrian Counting Sensors", align = "left"),
                 p("Pedestrian volumes are collected from a network of pedestrian counting sensors installed across the City of Melbourne."),
                 p("Data has been uploaded since 2009 every 15 to 20 minutes by sensors and is stored hourly, 24 hours a day. 18 sensors were initially installed in 2009, with additional sensors fitted over the years bringing the total number of installed sensors to 66 in June 2020."),
                 img(src = "Melbourne_City.png", width = "100%"),
                 em("Figure: Melbourne City Pedestrian Counting Sensors and Places of Interest"),
                 br()
                 ), 
        tabPanel("Project Summary",
                 h3("Project Information"),
                 strong("Project Team"),
                 p("Jessica Tong, Hans Gao, and Marcus Rzanovski"), 
                 strong("Project Supervisors"),
                 p("Patricia Sauri Lavieri and Andre Barbosa Nunes Da Cunha"),
                 em("Department of Infrastructure Engineering, The University of Melbourne"),
                 br(),
                 br(),
                 p("This app is part of our final-year Capstone Project for the Master of Engineering."),
                 p("The main results from our research are summarised in the 'Event Results' tab, where you can choose to view a specific event from a list of approximately 200 events from 2019 that occurred in and around Melbourne City that we have investigated. These results are drawn from an analysis of Melbourne City's Pedestrian Counting Sensors (see 'Pedestrian Sensor Information' tab for more information)."),
                 hr(),
                 # br(),
                 h4("Project Objectives"),
                 p("- Estimate how long an event lasts and the areas affected in a pedestrian network"),
                 p("- Detect anomalies in pedestrian counting sensors and map locations of outliers that occur during an event"),
                 p("- Use outliers detected during an event to estimate the event's temporal span of effect (how long the event lasts within the network)"),
                 p("- Determine the scale of event effect (increase/decreasee in the number of pedestrians)"),
                 hr(),
                 # br(),
                 h4("Project Outcomes"),
                 p("- 250 events investigated, with results shown for approximately 200 events"),
                 p("- Methods used could be extended to automatically detect future events, or to look at other historical events"),
                 p("- These results provide an overview of event effects that could be used to inform future city and event planning activities"),
                 br()
                 )
        )
      )
    )
)

# Define server logic ----
server <- function(input, output) {

  # Event Start Date-Time Input Selection
  # output$chosen_date <- renderUI({
  #   selectInput("chosen_date",
  #               label = "Date and scheduled start time of event (some events are recurring)",
  #               choices = unique(events[events$Event == input$chosen_event, ]$Event_StartString)
  #               )
  #   })
  

  # Event Name and Start Date for Filtering
  eventfilter <- reactive({
    events %>% filter(Event_ID == input$chosen_event)
    ### tibble("EventString" = input$chosen_event) %>%
    ###   left_join(events, by = c("EventString"))
  })
  
  # Print Event Information
  output$chosen_event_schedule <- renderText({
    paste0("Scheduled Duration: ",
           format(eventfilter()$Event_Date_Time_Start, "%b %d %H:%M"),
           " to ",
           format(eventfilter()$Event_Date_Time_End + hms("01:00:00"), "%b %d %H:%M"))
  })
  
  output$chosen_event_time <- renderText({
    paste0("Time of Day: ", eventfilter()$Time_Of_Day)
  })
  
  output$chosen_event_type <- renderText({
    paste0("Event Type: ", eventfilter()$Type)
  })
  
  output$chosen_event_recurring <- renderText({
    paste0("Recurring Event: ", eventfilter()$Recurring_Event)
  })
  
  output$chosen_event_area <- renderText({
    paste0("Event Location: ", eventfilter()$Area)
  })
  
  output$chosen_event_attendance <- renderText({
    paste0("Estimated Attendance: ", scales::comma(eventfilter()$Estimated_Attendance))
  })
  
  # Daily Comparison Plot
  output$plot_dailycomparison <- renderPlot({
    before_after %>%
    select(Sensor_ID, Date_Time, Event, 'Actual Pedestrian Count' = Hourly_Counts, '4-week Historical Average' = Avg4w, Event_Date_Time_Start, Event_Date_Time_End, StartEvent, EndEvent, Event2) %>%
    filter(date(Date_Time) >= date(eventfilter()$Event_Date_Time_Start[1]) & date(Date_Time) <= date(eventfilter()$Event_Date_Time_End[1])) %>%
    pivot_longer(cols = c("Actual Pedestrian Count", "4-week Historical Average"), names_to = "Type", values_to = "Count") %>%
    ggplot(aes(x = Date_Time)) +
    geom_rect(data = ~group_by(.x, Event2),
              aes(xmin = StartEvent, xmax = EndEvent,
                  ymin = min(Count), ymax = max(Count), fill = "event"), na.rm = TRUE) +
    scale_fill_manual(name = NULL, values = "grey", labels = "Scheduled Event") +
    geom_line(aes(y = Count, group = Sensor_ID, color = Type)) +
    facet_wrap(vars(Type), ncol = 1, nrow = 2) +
    scale_y_continuous(labels = scales::label_comma(), breaks = scales::breaks_extended(6), expand = c(0,0)) +
    scale_x_datetime(date_breaks = "6 hours", labels = scales::label_time(format = "%d %b\n %H:%M"), expand = c(0,0)) +
    labs(x = NULL, y = "No. of Pedestrians", color = "Type (All Sensors)", fill = "Schedulded Event") +
    theme_minimal() +
    guides(colour = guide_legend(nrow = 2, title.position = "left", byrow = FALSE)) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line.y =  element_line(),
          axis.ticks.y = element_line(),
          axis.line.x =  element_line(),
          axis.ticks.x = element_line(),
          strip.text.x = element_blank(),
          legend.position = "top",
          legend.box.just = "center"
    )
  })
  
  # Temporal Plot
  output$plot_temporal <- renderPlot({
    outlier_sensors %>% 
      filter(Event_ID == eventfilter()$Event_ID, Event_Date_Time_Start == eventfilter()$Event_Date_Time_Start) %>% 
    ### eventfilter() %>% 
    ###   left_join(outlier_sensors, by = c("Event", "Event_Date_Time_Start")) %>% 
      ggplot(aes(x= Start, y = Scale, colour = as.factor(Scale))) +
      geom_linerange(aes(xmin = Start, xmax = End + hms("01:00:00")), size = 10, na.rm = TRUE) +
      theme_classic() +
      scale_colour_manual(name = "Temporal Spans",
                          breaks = c("1", "2", "3", "4", "5"),
                          values = c("1" = scales::hue_pal()(5)[1],
                                     "2" = scales::hue_pal()(5)[2],
                                     "3" = scales::hue_pal()(5)[3],
                                     "4" = scales::hue_pal()(5)[4],
                                     "5" = scales::hue_pal()(5)[5]),
                          labels = c("1" = "Scheduled Event",
                                     "2" = "Event Temporal Span: Level 1 & 2 Outliers",
                                     "3" = "Level 3 Outliers",
                                     "4" = "Level 4 Outliers",
                                     "5" = "Level 5 Outliers"), 
                          drop = FALSE) +
      scale_x_datetime(date_breaks = "6 hours", labels = scales::label_time(format = "%d %b\n%H:%M")) +
      labs(x = "Time") +
      theme(
        axis.line.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "top",
        plot.margin = margin(0, 10, 0, 0, unit = "pt")
      )
  })
  
  # Outlier Plot
  output$plot_outliers <- renderLeaflet({
    leaflet(outlier_hour) %>%
      addProviderTiles(#providers$Stamen.TonerLite,
                      providers$CartoDB.Positron,
                       options = providerTileOptions(noWrap = TRUE)) %>% 
                        fitBounds(~min(Longitude), ~min(Latitude), ~max(Longitude), ~max(Latitude)) 
  })
  
  outliersfilter <- reactive({
    outlier_hour %>%
      filter(Event_ID == input$chosen_event) %>% 
      arrange(desc(Scale))
  })

  observe({
    leafletProxy("plot_outliers", data = outliersfilter()) %>%
      clearShapes() %>%
      addCircleMarkers(lng = ~Longitude, lat = ~Latitude, 
                       radius = ~(3^sqrt(as.numeric(levels(Scale))[Scale])), 
                       stroke = FALSE,
    fillColor = ~scales::hue_pal()(5)[as.numeric(levels(Scale))[Scale]],
    fillOpacity = ~as.numeric(levels(Scale))[Scale]*(-.3) + 1.6, weight = 2) %>% 
      addLegend(color = ~scales::hue_pal()(5)[2:5], 
                labels = ~levels(Scale))  
  })
  
  # output$plot_outliers <- renderPlot({
  #   # outliers <- eventfilter() %>%
  #     # left_join(outlier_hour, by = c("Event", "Event_Date_Time_Start"))
  # 
  #   # ggplot() +
  #   #   geom_sf(data = map_streetsped, inherit.aes =  FALSE, color = "black",
  #   #           size = 0.1, alpha = 0.2) +
  #   #   geom_sf(data = map_streets, inherit.aes = FALSE, color = "black",
  #   #           size = 0.3, alpha = 0.2) +
  #   #   geom_sf(data = map_rail, inherit.aes = FALSE, color = "grey",
  #   #           size = 0.2, linetype = "dotdash", alpha = 0.5) +
  #   #   geom_sf(data = outliers, aes(geometry = geometry, color = Scale, size = Scale)) +
  #   #   geom_sf(data = st_as_sf(sensorloc, coords = c("Longitude", "Latitude"), crs = 4326), aes(color = "L0", size = "L0")) +
  #   #   scale_size_manual(name = "Outliers",
  #   #                     breaks = c("2", "3", "4", "5", "L0"),
  #   #                     values = c("L0" = 1, "2" = 3, "3" = 5, "4" = 7, "5" = 9),
  #   #                     labels = c("2" = "Level 1 to 2", "3" = "Level 3", "4" = "Level 4", "5" ="Level 5", "L0" = "Pedestrian Sensors"),
  #   #                     drop = FALSE) +
  #   #   scale_colour_manual(name = "Outliers",
  #   #                       breaks = c("2", "3", "4", "5", "L0"),
  #   #                       values = c("L0" = "black",
  #   #                                  "2" = scales::hue_pal()(5)[2],
  #   #                                  "3" = scales::hue_pal()(5)[3],
  #   #                                  "4" = scales::hue_pal()(5)[4],
  #   #                                  "5" = scales::hue_pal()(5)[5]),
  #   #                       labels = c("2" = "Level 1 to 2", "3" = "Level 3", "4" = "Level 4", "5" ="Level 5", "L0" = "Pedestrian Sensors"),
  #   #                       drop = FALSE) +
  #   #   guides(color = guide_legend(nrow = 1, title.position = "left", byrow = FALSE)) +
  #   #   coord_sf(xlim = c(144.9380 , 144.9800), ylim = c(-37.8250, -37.7930), expand = FALSE) +
  #   #   theme_void() +
  #   #   theme(legend.position = "top", legend.box.just = "center", strip.text.x = element_text(vjust = 1))
  # })

  event_before_after <- reactive({
    event_before_after <- eventfilter() %>%
      left_join(before_after, by = c("Event", "Event_Date_Time_Start", "Event_Date_Time_End", "Event_Temporal_Start", "Event_Temporal_End", "Event_Temporal_Span", "Date"))
    
    event_cluster <- event_before_after %>%
      select(
        Sensor_ID, Latitude, Longitude, Date_Time, Data = Difference
      ) %>%
      pivot_wider(
        id_cols = -c(Data, Date_Time),
        names_from = Date_Time,
        values_from = c(Data),
        names_sep = "_"
      ) %>%
      mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))
  
    # Clustering
    sensor_cluster <- event_cluster %>%
      select(-c(Sensor_ID)) %>%
      # standardise each column
      scale() %>%
      kmeans(centers = input$ncluster, nstart = 50, iter.max = 100)
  
    event_cluster[["Cluster"]] <- sensor_cluster[["cluster"]]
  
    sensorgroups <- event_cluster %>%
      select(Sensor_ID, Cluster)
  
    event_before_after %>%
      left_join(sensorgroups, by = "Sensor_ID") %>%
      group_by(Cluster, Date_Time) %>%
      mutate(Difference_GroupAvg = mean(Difference[is.finite(Difference)], na.rm = TRUE)) %>%
      drop_na(Cluster) %>%
      ungroup() 
  })
  
  
  # Cluster Map
  output$plot_cluster <- renderPlot({
    sensor_cluster <- event_before_after() %>%
      left_join(sensorloc, by = c("Sensor_ID", "Latitude", "Longitude")) %>%
      st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
    
    ggplot() +
      geom_sf(data = map_streetsped, inherit.aes =  FALSE, color = "black",
              size = 0.2, alpha = 0.2) +
      geom_sf(data = map_streets, inherit.aes = FALSE, color = "black",
              size = 0.4, alpha = 0.2) +
      geom_sf(data = map_rail, inherit.aes = FALSE, color = "grey",
              size = 0.3, linetype = "dotdash", alpha = 0.5) +
      geom_sf(data = sensor_cluster, aes(color = factor(Cluster)), size = 3) +
      labs(colour = "Cluster") +
      guides(colour = guide_legend(nrow = 1, title.position = "left", byrow = FALSE)) +
      coord_sf(xlim = c(144.9380 , 144.9800), ylim = c(-37.8250, -37.7930), expand = FALSE) +
      theme_void() +
      theme(legend.position = "top", legend.box.just = "center")
  })
  
  # Events differences plot
  output$plot_scale <- renderPlot({
    event_before_after() %>%
      ggplot(aes(x = Date_Time)) +
      geom_line(aes(y = Difference, group = Sensor_ID, colour = factor(Cluster), linetype = "sensor")) +
      geom_line(aes(y = Difference_GroupAvg, linetype = "average")) +
      scale_linetype_manual(values = c("sensor" = "solid", "average" = "dotted"),
                            labels = c("sensor" = "Individual Sensor", "average" = "Group Average"),
                            breaks = c("average", "sensor")) +
      scale_y_continuous(labels = scales::label_comma(), breaks = scales::breaks_extended()) +
      scale_x_datetime(labels = scales::label_time(format = "%d %b\n %H:%M"),
                       breaks = function(x) c(x[1], x[2]),
                       expand = c(0,0)
      ) +
      labs(x = NULL,
           y = "Difference in number of Pedestrians: Event vs. Historical Average",
           colour = "Cluster",
           linetype = "Measure"
      ) +
      # guides(colour = guide_legend(nrow = 1, title.position = "left", byrow = FALSE),
      #        linetype = guide_legend(title.position = "left")
      # ) +
      theme_minimal() +
      theme(legend.position = "right",
            legend.box.just = "center",
            strip.text.y = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line.y =  element_line(),
            axis.ticks.y = element_line(),
            panel.spacing = unit(2.5, "lines"),
            plot.margin = margin(0, 10, 0, 0, unit = "pt")
      ) +
      coord_cartesian(clip = "off") +
      facet_wrap(vars(Cluster), ncol = 1)
  })

  # Event Summary
  # Outliers Detected
  
  # chosen_events_summary <- chosen_events %>%
  #   group_by(Event, Cluster) %>%
  #   summarise(Average_Difference = mean(Difference[is.finite(Difference)], na.rm = TRUE)) %>%
  #   ungroup() %>%
  #   pivot_wider(id_cols = Cluster, names_from = Event, values_from = Average_Difference)
  
}

# Run the app ----
shinyApp(ui = ui, server = server)