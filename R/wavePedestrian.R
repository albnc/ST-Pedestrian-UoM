## INSTALL PACKAGES -----------------------------
install.packages(c("WaveletComp", "lubridate", "tidyverse", "ggplot2"))

## LOAD PACKAGE ---------------------------------
library(tidyverse)
library(WaveletComp)
library(lubridate)
library(ggplot2)

## DATA -------------------------------------------
## Upload CSV file
csv <- read.csv("C:/Users/engan/GitHub/Pedestrian_Counting_System___2009_to_Present__counts_per_hour_.csv",
                   header = TRUE, sep = ",")

## Create a tibble dataset
pedata <- tibble(
  id = csv$ID,
  year = csv$Year,
  month = match(csv$Month,month.name),
  day = csv$Mdate,
  hour = csv$Time,
  sensorID = csv$Sensor_ID,
  sensorName = csv$Sensor_Name,
  count = csv$Hourly_Counts
)
## Time transformation
pedata <- pedata %>% 
  mutate( datetime = make_datetime(
    year = year,
    month = month,
    day = day,
    hour = hour)#,
    #tz = "Australia/Melbourne") # there is a problem with daylight time in October 2009
  ) 

## Amount of missing data
sum(is.na(pedata$datetime))

## Summary
summary(pedata)

## TIME ANALYSIS ------------------------------------------
## Compute cases per year
pedata %>% 
  count(year, wt=count)

## Compute cases per sensor
pedata %>% 
  count(sensorID, wt=count)

## Compute cases per year and sensor over years
pdt <- pedata %>% 
  group_by(sensorID, year) %>%
  summarise(avg = mean(count, na.rm = TRUE), total = sum(count, na.rm = TRUE))

pdt %>% 
  ggplot(aes(year, total)) +
  geom_line(aes(group = sensorID, colour = sensorID)) +
    labs(x = "Years", y = "Annual Count")
  
## Box plot over sensors
pedata %>% 
  filter(year == 2018) %>% 
  ggplot(aes(sensorID, count)) +
  geom_boxplot(aes(group = sensorID))

## Plot time series
pedata %>%
  filter(year == 2018) %>%
  group_by(sensorID, hour) %>% 
  summarise(avg = mean(count, na.rm = TRUE)) %>% 
  ggplot(aes(hour, avg)) +
  geom_line(aes(group = sensorID, colour = sensorID))
  
## WAVELET ANALYSIS ---------------------------------------
