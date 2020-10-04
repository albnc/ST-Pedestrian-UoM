library(tidyverse)
library(lubridate)
library(waveslim)

source("R/01-datafilter.R")
## All the analysis are considering only the data from 2019

# 1 - Temporal Analysis ---------------------------------------------------
# Formatting dataset
sensors <- ped.year %>%
  select(sensorID, lat, long, count, datetime) %>% 
  mutate(year=year(datetime),
         month=month(datetime, label=TRUE, abbr=TRUE),
         wday=weekdays(datetime, abbreviate=TRUE))
sensors <- sensors %>%
  group_by(sensorID, lat, long) %>% 
  nest()

# What are the count distribution through year, month, days of week? -------
## All Timeline
ggplot(sensors$data[[1]], aes(x=datetime, y=count)) + 
  geom_line()

## Boxplot by month
## Boxplot doesn't tell any outlier, it is defined by the user using the deviation
params$stdev <- 1.5 # standard deviation
ggplot(sensors$data[[13]], aes(x=month, y=count)) + 
  geom_boxplot(coef=params$stdev, outlier.color = "red", outlier.size = 1) +
  facet_wrap(~year)


## Timelines
p <- ggplot(sensors$data[[13]], 
            aes(x=hour, y=count, group=date(datetime), color=date(datetime))) +
  geom_line()

## Timeline by months
p + facet_wrap(~month)

## Timeline by weekdays
p + facet_wrap(~wday)

## Timeline by month and weekdays
p + facet_grid(rows=vars(month), col=vars(wday))

## There are a pattern during weekdays, however some sensors registered some
## anomalies on weekdays. Others present high variation in the first three 
## months of the year. 


# 2 - Wavelet Analysis ----------------------------------------------------

# What are the anomalies in one year? -------------------------------------

sensors.wav <- sensors.full %>% 
  mutate(
  modwt = map(data, ~{mra(.x$count, wf="haar", J=5, method="modwt")})
)

sid <- 5
y <- c(2019)
idy <- sensors.wav$data[[sid]]$year %in% y
dt <- sensors.wav$modwt[[sid]]$D3[idy]
thresh <- sd(dt) * sqrt(2 * log(length(dt)))
idt <- abs(dt) > thresh

dt <- sensors.wav$data[[sid]] %>% filter(idy)
ggplot(dt, aes(x=datetime, y=count)) +
  geom_line(color="grey") +
  geom_point(data = dt %>%  filter(idt), color="red")

# 3 - Spatial Analysis ----------------------------------------------------
# Where are the sensors? --------------------------------------------------
library(leaflet)

## Add map with sensor position
## Function to colors
getColor <- function(s){
  sapply(s$fillHour, function(hours){
    if (hours <= 2 * 24){
      "green"
    } else if (hours <= 60 * 24){
      "orange"
    }
    else{
      "red"
    }
  })
}

icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = getColor(sensors)
)

leaflet(data=sensors) %>% 
  addAwesomeMarkers(lng=~longitude, lat=~latitude, label = ~sensorID, icon = icons) %>% 
  # addMarkers(lng=~longitude, lat=~latitude, label = ~sensorID, icon = icons) %>% 
  # addTiles()
  # addProviderTiles(providers$Stamen.Toner)
  addProviderTiles(providers$CartoDB.Positron)
  # addProviderTiles(providers$Esri.NatGeoWorldMap)
  
