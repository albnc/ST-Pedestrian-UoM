library(tidyverse)
library(lubridate)
library(purrr)
library(waveslim)

###########################################################
## LOAD PEDESTRIAN DATA
## output:  tibble grouped by sensor ID
##
load_pedata <- function(idfilter = NULL) {
  ## DATA -------------------------------------------
  ## Upload CSV file
  csv <- read.csv("data/Pedestrian_Counting_System___2009_to_Present__counts_per_hour_.csv",
                  header = TRUE, sep = ",")
  
  pos <- read.csv("data/Pedestrian_Counting_System_-_Sensor_Locations.csv", header = TRUE, sep=",")
  
  db <- merge(csv, pos, by.x = "Sensor_ID", by.y="sensor_id")
  
  ## Create a tibble dataset
  pedata <- tibble(
    id = db$ID,
    year = as.factor(db$Year),
    month = as.factor(match(db$Month,month.name)),
    day = as.factor(db$Mdate),
    hour = as.factor(db$Time),
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
  paste("Missing data: ", sum(is.na(pedata$datetime)))
  
  ## Nest data grouping by sensor ID
  sensors <- pedata %>% 
    arrange(sensorID, datetime) %>% 
    group_by(sensorID) %>% 
    nest()
  
  ## Filtering data
  if(!is.null(idfilter)) {
    sensors <- sensors %>% 
      filter(sensorID %in% idfilter)
  }
  
  return(sensors)
}


###########################################################
## WAVELET DECOMPOSITION ANALYSIS
## output:  decomposition signal in approximation and detail
##          coefficients
##
wavlevels <- function(signal, wfam="la8", wtype="modwt"){
  ## Level 3 (8h) is the maximum level to analyze in Wavelet decomposition for
  ## pedestrian hourly data
  nlevel = 3
  colnames = c()
  
  ## Wavelet analysis
  for (i in 1:nlevel) {
    ## MRA - MultiResolution Analysis 
    wav <- mra(x=signal$count, wf=wfam, J = i, method = wtype)
    colname <- paste("s",i, sep="") 
    signal[[colname]] <- with(signal, wav[[length(wav)]])
  }
  
  df <- signal %>% 
    select(datetime, count, num_range("s",1:nlevel)) %>% 
    pivot_longer(!datetime, names_to = "typesignal", values_to = "value")
  
  ## Plot
  ggplot(df, mapping=aes(x=datetime, y=value, colour=typesignal)) +
    geom_point() +
    geom_line() +
    ylim(min(df$value), max(df$value))
  
}