library(tidyverse)
library(lubridate)
library(purrr)


###########################################################
## LOAD PEDESTRIAN DATA
## output:  tibble grouped by sensor ID
##
load_pedata <- function(input, idfilter=NULL) {
  ## DATA -------------------------------------------
  ## Upload CSV file
  csv <- read.csv("C:/Users/engan/GitHub/Pedestrian_Counting_System___2009_to_Present__counts_per_hour_.csv",
                  header = TRUE, sep = ",")
  
  pos <- read.csv("./_data/Pedestrian_Counting_System_-_Sensor_Locations.csv", header = TRUE, sep=",")
  
  db <- merge(csv, pos, by.x = "Sensor_ID", by.y="sensor_id")
  
  ## Create a tibble dataset
  pedata <- tibble(
    id = db$ID,
    year = db$Year,
    month = match(db$Month,month.name),
    day = db$Mdate,
    hour = db$Time,
    sensorID = db$Sensor_ID,
    sensorName = db$Sensor_Name,
    count = db$Hourly_Counts,
    lat = db$latitude,
    long = db$longitude
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
  print(c("Missing data: ", sum(is.na(pedata$datetime))))
  
  ## Nest data grouping by sensor ID
  sensors <- pedata %>% 
    group_by(sensorID) %>% 
    nest()
  
  ## Filtering data
  if(!is.null(idfilter)) {
    sensors <- filter(sensors, sensorID == idfilter)
  }
  
  return(sensors)
}
