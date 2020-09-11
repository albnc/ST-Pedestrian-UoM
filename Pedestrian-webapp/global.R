library(dplyr)
library(ggplot2)
library(leaflet)
library(lubridate)

# Loading the dataset
ped.data <- read.csv("../data/sample2019.csv")
ped.data <- ped.data %>% 
  mutate(datetime = make_datetime(year, month, day, hour, 0)) %>% 
  select(sensorID, lat, long, datetime, count)

## Compute a summary for mapping
ped.summary <- ped.data %>% 
  group_by(sensorID, lat, long) %>% 
  summarise(total=sum(count), avg=mean(count), stdev=sd(count))

