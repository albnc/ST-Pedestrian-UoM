library(tidyverse)
library(lubridate)
library(waveslim)

## Change working directory
setwd("appPed")
wdpath <- getwd()

# Import data -------------------------------------------------------------
## Upload CSV file wity counting
csv <- read.csv(paste0(wdpath, "/data/Pedestrian_Counting_System___2009_to_Present__counts_per_hour_.csv"),
                header = TRUE, sep = ",")
## Upload sensor positions
pos <- read.csv(paste0(wdpath, "/data/Pedestrian_Counting_System_-_Sensor_Locations.csv"), 
                header = TRUE, sep=",")
## Standardize the column "sensorID"
pos <- rename(pos, sensorID = sensor_id)

# Create a tibble dataset --------------------------------------------------------
pedata <- tibble(
  id = csv$ID,
  year = csv$Year,
  month = as.factor(match(csv$Month,month.name)),
  day = csv$Mdate,
  hour = csv$Time,
  sensorID = csv$Sensor_ID,
  count = csv$Hourly_Counts,
) %>% 
## Time transformation
  mutate( datetime = make_datetime(
    year = year,
    month = as.numeric(month),
    day = day,
    hour = hour),
    #tz = "Australia/Melbourne") # there is a problem with daylight time in October 2009
    # year = as.factor(year),
    # month = as.factor(month),
    # day = as.factor(day),
    # hour = as.factor(hour)
  ) %>% 
  ## weekdays as a factor
  mutate(wday=wday(datetime,TRUE)) 

## Abbreviate names as level
levels(pedata$month) <- month.abb 

## Nest data grouping by sensor ID
sensors <- pedata %>% 
  arrange(sensorID, datetime) %>% 
  group_by(sensorID) %>% 
  nest()

sensors <- inner_join(sensors,
           pos %>% select(sensorID, latitude, longitude), 
           by="sensorID")

## Remove older variables
rm(list = c("csv", "pos", "pedata"))

# Is there any difference using Australian time zone? ---------------------
# I don't know. Talk to the students


# Are there missing values in datetime? -----------------------------------
countHours <- function(idx) {
  temp <- tibble(
    datetime=seq.POSIXt(from = head(sensors$data[[idx]]$datetime,1),
                     to = tail(sensors$data[[idx]]$datetime,1),
                     by = "1 hour")
    )
  diffData <- nrow(temp) - nrow(sensors$data[[idx]])
  print(paste0("There are ", diffData," missing hours (~", 
               round(diffData/24,2) , " days)"))
  
  newdata <- full_join(x=sensors$data[[idx]],
                        y=temp,
                        by="datetime")
  return (diffData)
  #return(newdata)
}

sensors <- sensors %>% 
  mutate(fillHour=countHours(sensorID))

sensors %>% arrange(fillHour)

ggplot(sensors, aes(x=reorder(sensorID, -fillHour), y=fillHour)) + 
  geom_col() + coord_flip()

sensors.full <- filter(sensors, fillHour < 50)

# There are some missing values:
# 1. In some days, all 24 hourly counts were saved as 00:00 AM. It's
# necessary to rebuild this;
# 2. During summer time, reducing the hour, the data is lost. When add one hour
# the data is duplicated.


# 1 - Temporal Analysis ---------------------------------------------------

# What are the count distribution through year, month, days of week? -------
## All Timeline
ggplot(sensors.full$data[[1]], aes(x=datetime, y=count)) + 
  geom_line()

## Boxplot by month
stdev <- 1.5 # standard deviation
ggplot(sensors.full$data[[13]], aes(x=month, y=count)) + 
  geom_boxplot(coef=stdev, outlier.color = "red", outlier.size = 1) +
  facet_wrap(~year)


## Timelines
p <- ggplot(sensors.full$data[[13]] %>% filter(year==2019), 
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
  
