library(tidyverse)
library(lubridate)
library(purrr)


# Load data -------------------------------------------------------------------------
## Filenames
datafile = "data/pedestrian_counts_2009-2019.csv"
#datafile = "../data/pedestrian_counts_2009-2019.csv"
posfile = "data/sensor_locations.csv"

## Upload CSV files
csv.data <- read.csv(datafile, header = TRUE, sep = ",")
csv.pos <- read.csv(posfile, header = TRUE, sep=",")

## Create dataset
db <- merge(csv.data, csv.pos, by.x = "Sensor_ID", by.y="sensor_id")

## Create a tibble dataset
pedata <- tibble(
  id = db$ID,
  sensorID = db$Sensor_ID,
  sensorName = db$Sensor_Name,
  count = db$Hourly_Counts,
  long = db$longitude,
  lat = db$latitude
) %>% 
  mutate( datetime = make_datetime(
    year = db$Year,
    month = match(db$Month,month.name),
    day = db$Mdate,
    hour = db$Time),
    datetimeTZ = make_datetime(
      year = db$Year,
      month = match(db$Month,month.name),
      day = db$Mdate,
      hour = db$Time,
      tz = "Australia/Melbourne")
  ) %>% 
  arrange(sensorID, datetime)


# Identify missing and duplicate data -----------------------------------------------
## How many missing date are in the dataset?
sum(is.na(pedata$datetime))
sum(is.na(pedata$datetimeTZ))
miss.data <- pedata[is.na(pedata$datetimeTZ),]

## The date with NA values
dt <- pedata %>% 
  filter(month(datetime) %in% c(4, 10),
         day(datetime) <=7,
         weekdays(datetime, abbreviate = TRUE)=="Sun",
         hour(datetime) %in% c(1:3))
dt
## All the missing data using TZ were from the daylight savings. So, I'm considering to eliminate those rows with 'NA'
pedata <- pedata %>% 
  select(!datetime) %>%
  drop_na() %>% 
  rename(datetime=datetimeTZ)

# Filtering data --------------------------------------------------------------------
## Filter year 2019
params <- tibble(year = 2019) 
ped.year <- pedata %>% 
  select(sensorID, sensorName, count, long, lat, datetime) %>% 
  filter(year(datetime) == params$year) %>% 
  arrange(sensorID, datetime)

rm(list=c("csv.data", "csv.pos", "db", "datafile", "posfile", "miss.data", "dt"))
#ped.year %>% glimpse


# Summary data ----------------------------------------------------------------------
ped.summary <- ped.year %>% 
  group_by(sensorID, long, lat) %>% 
  summarise(count.tot=sum(count), count.avg=mean(count), count.std=sd(count), perc.year=n()/(24*yday(make_datetime(params$year, 12, 31)))) %>% 
  ungroup()
ped.summary

## Plot missing data per sensor
ped.summary %>% arrange(perc.year)

ggplot(ped.summary, aes(x=reorder(sensorID, -perc.year), y=perc.year)) + 
  geom_col() + coord_flip() +
  ylab("Percentage of data in the year") + xlab("Sensor ID")
