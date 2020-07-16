library(tidyverse)
library(lubridate)

load_pedata <- funtion(input, sensors='all'){
  ## DATA -------------------------------------------
  ## Upload CSV file
  csv <- read.csv("C:/Users/engan/GitHub/Pedestrian_Counting_System___2009_to_Present__counts_per_hour_.csv",
                  header = TRUE, sep = ",")
  
  ## Create a tibble data
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
  print(c("Missing data: ", sum(is.na(pedata$datetime))))
  
  ## Filtering data
  if(sensors == 'all'){
    filtered <- pedata
  }
  else{
    filtered <- filter(pedata, id == sensors)
  }
  
  
  return(filtered)
}