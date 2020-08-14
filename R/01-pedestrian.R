library(tidyverse)
library(lubridate)

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


