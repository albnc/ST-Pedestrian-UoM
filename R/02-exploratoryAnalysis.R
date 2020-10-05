library(waveslim)

source("R/01-datafilter.R")
## All the analysis are conparams$idering only the data from 2019

# 1 - Temporal Analysis ---------------------------------------------------
# Formatting dataset
sensors <- ped.year %>%
  select(sensorID, lat, long, count, datetime) %>% 
  mutate(year=year(datetime),
         month=month(datetime, label=TRUE, abbr=TRUE),
         wday=factor(weekdays(datetime, abbreviate=TRUE),labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
         hour=hour(datetime))
sensors <-  sensors %>%
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
            aes(x=hour, y=count, group=date(datetime), color=wday)) +
  geom_line()

## Timeline by months
p + facet_wrap(~month)

## Timeline by weekdays
p + facet_wrap(~wday)

## Timeline by month and weekdays
p + facet_grid(rows=vars(month), col=vars(wday))

## There are a pattern during weekdays, apparently Wednesday and Thursday are a different pattern from the other days of the week.
## Some sensors registered some anomalies on weekdays. Others present high variation in the first three months of the year. 


# 2 - Wavelet Analysis ----------------------------------------------------

# What are the anomalies in one year? -------------------------------------
params$wav.level = 5 # Correspond to 32h
sensors <- sensors %>% 
  mutate(
  modwt = map(data, ~{mra(.x$count, wf="haar", J=params$wav.level, method="modwt")})
)

params$id <- 5
params$level <- 'D2'
coef.details <- sensors.wav$modwt[[params$id]][[params$level]]
## Threshold global
thresh <- sd(coef.details) * sqrt(2 * log(length(coef.details)))
idt <- abs(coef.details) > thresh

dt <- sensors.wav$data[[params$id]]
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

leaflet(data=ped.summary) %>% 
  fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat)) %>%
  #addAwesomeMarkers(lng=~long, lat=~lat, label = ~sensorID, icon = icons) %>% 
  addCircleMarkers(lng=~long, lat=~lat, radius = ~sqrt(count.avg)/3, label = ~sensorID) %>% 
  # addMarkers(lng=~longitude, lat=~latitude, label = ~sensorID, icon = icons) %>% 
  # addTiles()
  # addProviderTiles(providers$Stamen.Toner)
  addProviderTiles(providers$CartoDB.Positron)
  # addProviderTiles(providers$Esri.NatGeoWorldMap)
  
